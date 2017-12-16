package org.bykn.bosatsu;

public class EnumValue {
  final int variant;
  final Object[] values;

  public EnumValue(int v, Object[] vs) {
    variant = v;
    values = vs;
  }
}
