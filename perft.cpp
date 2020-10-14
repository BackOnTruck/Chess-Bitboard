// perft test program
#include "board.hpp"
int main()
{
	for(string s;;)
	{
		printf("%s\n\nFen (Enter to get default, Ctrl+D to quit) >>> ",fenf);
		//if(!getline(cin,s)){puts("\n\nQuitting...");break;}
		if(s.empty())s="rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
		board b(s);
		for(int i=6;i<7;i++)printf("Perft %d = %10d\n",i,b.perft(i))
			,fprintf(stderr,"Time used = %.3f\n",1.*clock()/CLOCKS_PER_SEC);
	}
}
