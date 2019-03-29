package org.bykn.bosatsu

import org.scalatest.FunSuite

class Regressions extends FunSuite {
  import TestUtils._
  import Evaluation.Value._

  test("example from https://github.com/johnynek/bosatsu/issues/196") {
    evalTestJson(
      List(
"""package Example/Resuitset

struct Resultset(data, fields)

struct Field(name: String, accessor: a -> b)

struct Row(firstName: String, lastName: String, age: Int)

def firstName(row: Row) -> String:
  Row(result, _, _) = row
  result

def lastName(row: Row) -> String:
  Row(_, result, _) = row
  result

def age(row: Row) -> Int:
  Row(_, _, result) = row
  result

fields = (Field("first", firstName),
  Some((Field("last", lastName),
    Some((Field("age", age),
      None)))))

rs = Resultset([Row("a", "b", 1), Row("c", "d", 2)], fields)

def applyFields(fields, row):
  recur fields:
    (field, Some(s)):
      Field(_, fn) = field
      (fn(row), Some(applyFields(s, row)))
    (field, None):
      Field(_, fn) = field
      (fn(row), None)

def materialize(rs):
  Resultset(rows, fields) = rs
  #[applyFields(fields, row) for row in rows]
  rows.map_List(\row -> applyFields(fields, row))

def addField(resultset, name: String, fn: a -> b):
  Resultset(old_data, old_fields) = resultset
  new_fields = (Field(name, fn), Some(old_fields))
  Resultset(old_data, new_fields)

def filterRecords(records, fn: a -> Bool):
  records.flat_map_List(\row ->
    keep = fn(row)
    match keep:
      True: [row]
      False: [])

def filter(rs, fn: a -> Bool):
  Resultset(old_records, same_fields) = rs
  new_records = filterRecords(old_records, fn)
  Resultset(new_records, same_fields)

def new_age(row):
  row.age.add(10)

new_resultset = rs.addField("newAge", new_age)
filtered_resultset = new_resultset.filter(\row -> eq_Int(row.new_age, 11))

out = materialize(filtered_resultset)
"""), "Example/Resuitset", Json.JString("foo"))
  }

  test("test complex recursion case from #196") {
    evalTest(List("""
package Foo

struct Field(name: String, extract: a -> b)

def applyFields(fields, row):
  recur fields:
    (f, Some(s)):
      Field(_, fn) = f
      rec = applyFields(s, row)
      (fn(row), Some(rec))
    (f, None):
      Field(_, fn) = f
      (fn(row), None)

hlist = (Field("a", \x -> "a"), Some((Field("b", \x -> "b"), None)))
main = applyFields(hlist, 1)
"""), "Foo", VInt(42))
  }
}
