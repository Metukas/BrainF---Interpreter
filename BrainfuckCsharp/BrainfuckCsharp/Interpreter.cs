using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BrainfuckCsharp
{
    class Interpreter
    {
        int instTapeLenght;

        int DataPointer;
        int InstructionPointer;
        Stack<int> LoopState;
        byte[] DataTape;
        string InstructionTape;

        public Interpreter(string instructionTape)
        {
            DataPointer = 0;
            InstructionPointer = 0;
            LoopState = new Stack<int>();
            instTapeLenght = instructionTape.Length;
            DataTape = new byte[1000_000];
            InstructionTape = instructionTape;
        }

        ref byte GetByteAttCurrentPtr() => ref DataTape[DataPointer];
        void IncrementPointer()
        {
            DataPointer++;
        }
        void DecrementPointer()
        {
            DataPointer--;
        }
        void IncrementData()
        {
            ref byte currentData = ref GetByteAttCurrentPtr();
            currentData++;
        }
        void DecrementData()
        {
            ref byte currentData = ref GetByteAttCurrentPtr();
            currentData--;
        }
        void Output()
        {
            char currentData = (char)GetByteAttCurrentPtr();
            Console.Write(currentData);
        }
        void Input()
        {
            ref byte currentData = ref GetByteAttCurrentPtr();
            var input = Console.Read();
            currentData = (byte)input;
        }

        void StartLoop___()
        {
         
            if (this.LoopState.Count == 0) // jeigu nėra jokio loopo
            {
                if (GetByteAttCurrentPtr() > 0) // jeigu nenulinė ląstelė...
                {
                    LoopState.Push(InstructionPointer); //...tai pradedam loopą
                }
                else // jei nulinė tai skipinam visą loopą
                {
                    LoopState.Push(-1);
                }
            }
            else // jeigu yra loopas
            {
                if (LoopState.Peek() < 0) // jeigu skipinam išorinį loopą...
                {
                    LoopState.Push(-1); // ... tada skipinam ir vidinį loopą
                }
                else if (GetByteAttCurrentPtr() > 0)
                {
                    LoopState.Push(InstructionPointer);
                }
                else
                {
                    LoopState.Push(-1);
                }
            }
        }

        void StartLoop()
        {
            // loopState neigiamas bus skip to loop end, o teigiamas - loopo pradžios indexas	
            if (LoopState.Count != 0 && LoopState.Peek() < 0) // jeigu jau yra skip to loop end, reiškia čia prasideda     vidinis loopas, kurį irgi skipinam
            {
                LoopState.Push(-1);
            }
            else if (GetByteAttCurrentPtr() > 0) // jeigu duomenys, kur rodo pointeris nėra lygūs 0
            {
            	LoopState.Push(InstructionPointer);
            }
            // skip to end loop
            else if (GetByteAttCurrentPtr() == 0)
            {
            	LoopState.Push(-1);
            }
        }
        void EndLoop__()
        {
            if(LoopState.Count == 0) // negalima baigti neprasidėjusio loopo
            {
                throw new Exception("negalima baigti neprasidėjusio loopo!!");
            }
            if(LoopState.Peek() < 0) // jeigu skipinam loopą
            {
                LoopState.Pop();
            }
            else if(LoopState.Peek() >= 0) // jeigu executinam loopą
            {
                if(GetByteAttCurrentPtr() > 0) // grįžtam atgal į loopo pradžią
                {
                    InstructionPointer = LoopState.Peek();
                }
                else // baigiam loopą
                {
                    LoopState.Pop();
                }
            }
        }

        void EndLoop()
        {
            if (LoopState.Count == 0)
            {
                throw new Exception("error: LoopState.Count == 0");
            }
            // skip to end loop
            if (LoopState.Peek() < 0)
            {
                LoopState.Pop();
            }
            else if (LoopState.Peek() >= 0)
            {
                if (GetByteAttCurrentPtr() > 0)
                {
                    InstructionPointer = LoopState.Peek();
                }
                else
                {
                    LoopState.Pop();
                }
            }
        }

        void MoveNextInstruction()
        {
            char nextInstructionChar = InstructionTape[InstructionPointer];
            if (nextInstructionChar != '[' && nextInstructionChar != ']' && LoopState.Count > 0 && LoopState.Peek() < 0)
            {
                InstructionPointer++;
                return;
            }
            switch(nextInstructionChar)
            {
                case '>':
                    IncrementPointer();
                    break;
                case '<':
                    DecrementPointer();
                    break;
                case '+':
                    IncrementData();
                    break;
                case '-':
                    DecrementData();
                    break;
                case '.':
                    Output();
                    break;
                case ',':
                    Input();
                    break;
                case '[':
                    StartLoop();
                    break;
                case ']':
                    EndLoop();
                    break;
                default:
                    break;
            }
            InstructionPointer++;
        }

        public void ExecuteToEnd()
        {
            while(InstructionPointer != instTapeLenght)
            {
                MoveNextInstruction();
            }
        }
    }
}
