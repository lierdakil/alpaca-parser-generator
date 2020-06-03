using System;
using lexer;
using parser;

namespace program
{
  class Program
  {
    static void Main(string[] args)
    {
      var debug = args.Length > 0 && args[0] == "-d";
      var line = Console.ReadLine();
      while(line != null) {
        var lex = new Lexer(line, debug);
        var parser = new Parser(lex, debug);
        var expr = parser.parse();
        Console.WriteLine($"Result: {expr}");
        line = Console.ReadLine();
      }
    }
  }
}
