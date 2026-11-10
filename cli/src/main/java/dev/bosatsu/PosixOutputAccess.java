package dev.bosatsu;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.NativeLong;
import com.sun.jna.Pointer;
import java.io.IOException;
import java.net.URLDecoder;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.UserDefinedFileAttributeView;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.nativeimage.PinnedObject;
import org.graalvm.nativeimage.Platform;
import org.graalvm.nativeimage.Platforms;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.PointerBase;
import org.graalvm.word.SignedWord;
import org.graalvm.word.UnsignedWord;
import org.graalvm.word.WordFactory;

/** NIO's POSIX view omits extended ACLs on Darwin and Linux. Query libc
 * without opening the artifact for reading or invoking a host executable.
 * Native Image links the same calls directly, including in the static CLI.
 */
final class PosixOutputAccess {
    private PosixOutputAccess() {}

    // copyfile.h: inspect only the ACL, without copying data or metadata.
    private static final int CHECK_ACL = (1 << 16) | 1;

    private interface Darwin extends Library {
        Darwin LIB = Native.load("c", Darwin.class);
        int copyfile(byte[] path, Pointer dest, Pointer state, int flags);
    }

    private interface Linux extends Library {
        Linux LIB = Native.load("c", Linux.class);
        NativeLong listxattr(byte[] path, byte[] names, NativeLong size);
    }

    static void preserve(Path source, Path staged) throws IOException {
        var oldNames = userAttributes(source);
        var stagedNames = userAttributes(staged);
        if (oldNames.isEmpty() && stagedNames.isEmpty()) return;
        var old = Files.getFileAttributeView(source, UserDefinedFileAttributeView.class);
        var next = Files.getFileAttributeView(staged, UserDefinedFileAttributeView.class);
        if (old == null || next == null)
            throw new IOException("cannot preserve output attributes: " + source);
        for (String name : stagedNames) {
            if (!oldNames.contains(name)) next.delete(name);
        }
        for (String name : oldNames) {
            ByteBuffer value = ByteBuffer.allocate(old.size(name));
            int read = old.read(name, value);
            if (read != value.capacity() || next.write(name, value.flip()) != read)
                throw new IOException("cannot copy output attribute " + name + ": " + source);
        }
    }

    private static Set<String> userAttributes(Path path) throws IOException {
        boolean darwin = switch (System.getProperty("os.name")) {
            case "Mac OS X" -> true;
            case "Linux" -> false;
            default -> throw new IOException(
                    "cannot inspect POSIX output access controls: " + path);
        };
        // The POSIX view exposes the nine rwx bits, not set-id or sticky bits.
        if (!(Files.getAttribute(path, "unix:mode") instanceof Integer mode)
                || (mode & 07000) != 0) {
            throw new IOException("cannot preserve special output mode: " + path);
        }
        // Unix file URIs preserve the raw filename bytes, even for a symlink
        // referent that cannot be decoded into a Java String. URLDecoder is a
        // form decoder, so protect literal plus signs before decoding escapes.
        String raw = path.toUri().getRawPath().replace("+", "%2B");
        String octets = URLDecoder.decode(raw, StandardCharsets.ISO_8859_1);
        byte[] name = (octets + '\0').getBytes(StandardCharsets.ISO_8859_1);
        long size = inspect(name, darwin, null);
        if (size < 0) throw new IOException("cannot inspect output access controls: " + path);
        if (size == 0) return Set.of();
        // Linux returns NUL-terminated names from all namespaces. Only user.*
        // is available for copying through NIO. Refuse an opaque ACL, security
        // label, or a name we cannot represent before exposing new bytes.
        if (!darwin && size <= 65536) { // Linux XATTR_LIST_MAX
            byte[] names = new byte[(int) size];
            if (inspect(name, false, names) != size)
                throw new IOException("output attributes changed during inspection: " + path);
            // Match the Unix NIO provider's encoding for user attribute names.
            var encoding = Charset.forName(System.getProperty("sun.jnu.encoding"));
            String list = encoding.newDecoder().decode(ByteBuffer.wrap(names)).toString();
            if (list.endsWith("\0")) {
                var user = new HashSet<String>();
                for (String attr : list.split("\0")) {
                    if (!attr.startsWith("user.")) {
                        throw new IOException(
                                "cannot preserve output access controls (" + attr + "): " + path);
                    }
                    user.add(attr.substring("user.".length()));
                }
                return user;
            }
        }
        throw new IOException("cannot preserve extended output access controls: " + path);
    }

    private static long inspect(byte[] path, boolean darwin, byte[] names) throws IOException {
        if (ImageInfo.inImageCode()) return Linked.inspect(path, names);
        try {
            if (darwin) return Darwin.LIB.copyfile(path, null, null, CHECK_ACL);
            var size = new NativeLong(names == null ? 0 : names.length);
            return Linux.LIB.listxattr(path, names, size).longValue();
        } catch (LinkageError e) {
            throw new IOException("cannot load native POSIX access check", e);
        }
    }

    @CContext(Headers.class)
    private static final class Linked {
        @Platforms(Platform.DARWIN.class)
        @CFunction("copyfile")
        private static native int copyfile(
                CCharPointer path, PointerBase dest, PointerBase state, int flags);

        @Platforms(Platform.LINUX.class)
        @CFunction("listxattr")
        private static native SignedWord listxattr(
                CCharPointer path, PointerBase names, UnsignedWord size);

        private static long inspect(byte[] path, byte[] names) {
            try (var name = CTypeConversion.toCBytes(path)) {
                if (Platform.includedIn(Platform.DARWIN.class)) {
                    return copyfile(name.get(), WordFactory.nullPointer(),
                            WordFactory.nullPointer(), CHECK_ACL);
                }
                if (Platform.includedIn(Platform.LINUX.class)) {
                    if (names == null) {
                        return listxattr(name.get(), WordFactory.nullPointer(),
                                WordFactory.zero()).rawValue();
                    }
                    try (var pinned = PinnedObject.create(names)) {
                        return listxattr(name.get(), pinned.addressOfArrayElement(0),
                                WordFactory.unsigned(names.length)).rawValue();
                    }
                }
                return -1;
            }
        }
    }

    static final class Headers implements CContext.Directives {
        @Override
        public List<String> getHeaderFiles() {
            if (Platform.includedIn(Platform.DARWIN.class)) return List.of("<copyfile.h>");
            if (Platform.includedIn(Platform.LINUX.class)) return List.of("<sys/xattr.h>");
            return List.of();
        }
    }
}
