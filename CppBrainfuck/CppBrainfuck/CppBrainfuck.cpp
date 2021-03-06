// CppBrainfuck.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stack>
#include <iostream>

unsigned char tape[30000] = { 0 };

unsigned char* ptr = tape;


void interpret(const char* input) {
	char current_char;
	size_t i;
	size_t loop;
	std::stack<int> loopState;

	for (i = 0; input[i] != 0; i++) {
		current_char = input[i];
		if (!loopState.empty() && loopState.top() < 0 && (current_char != ']' && current_char != '[') )
		{
			continue;
		}
		switch (current_char)
		{
			case '>':
				++ptr;
				break;
			case '<':
				--ptr;
				break;
			case '+':
				++*ptr;
				break;
			case '-':
				--*ptr;
				break;
			case '.':
				std::cout << *ptr;
				std::cout.flush();
				break;
			case ',':
				*ptr = getchar();
				break;
			case '[':
				// loopState neigiamas bus skip to loop end, o teigiamas - loopo pradžios indexas

				if (!loopState.empty() && loopState.top() < 0) // jeigu jau yra skip to loop end, reiškia čia prasideda vidinis loopas, kurį irgi skipinam
				{
					loopState.push(-1);
					continue;
				}
				if (*ptr) // jeigu duomenys, kur rodo pointeris nėra lygūs 0
				{
					loopState.push(i);
					continue;
				}
				// skip to end loop
				if (*ptr == 0)
				{
					loopState.push(-1);
					continue;
				}
				break;
			case ']':
				if (loopState.empty())
				{
					std::cout << "Error: loopState.empty()" << std::endl; // handlint reiktų kažkaip errorą
				}
				// skip to end loop
				if (loopState.top() < 0)
				{
					loopState.pop();
					continue;
				}
				if (loopState.top() >= 0)
				{
					if (*ptr)
					{
						i = loopState.top();
						//i++; // ar reikia?
						continue;
					}
					else
					{
						loopState.pop();
						continue;
					}
				}
				break;
			default:
				break;
		}
	}
}

#include <fstream>
#include <streambuf>


int main() 
{
	std::ifstream t("C:\\Users\\Matas\\source\\repos\\CppBrainfuck\\Debug\\file.txt");
	std::string str;

	t.seekg(0, std::ios::end);
	str.reserve(t.tellg());
	t.seekg(0, std::ios::beg);

	str.assign((std::istreambuf_iterator<char>(t)),
		std::istreambuf_iterator<char>());

	interpret(str.c_str());  // outputs input
	return 0;
}