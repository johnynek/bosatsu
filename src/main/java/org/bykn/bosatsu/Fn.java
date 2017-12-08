package org.bykn.bosatsu;

public interface Fn<A, B> {
  B apply(A arg);
}
