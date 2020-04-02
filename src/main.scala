package zone.slice.bezier

object Main {
  def main(args: Array[String]): Unit = {
    parse("""
      main() = {
        print("hey"). print("what's up")
        print("woop")

        print(
          "multi",
          "line",
          "drifting"
        )
      }

      add(x, y) = {
        print(x, y)
      }
    """)
  }
}
