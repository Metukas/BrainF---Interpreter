using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace BrainfuckCsharp
{
    class Program
    {
        static void Main(string[] args)
        {
            string fileContents = File.ReadAllText("brainfuck.txt");
            Interpreter interpreter = new Interpreter(fileContents);
            Console.WriteLine("Begin");
            interpreter.ExecuteToEnd();
        }
    }
}
