package org.bykn.bosatsu.hashing

case class Hashed[A, +Of](hash: HashValue[A], arg: Of)