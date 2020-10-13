// perft test program
#include "board.hpp"

int main()
{
	for(string s;;)
	{
		printf("%s\n\nFen (Enter to get default, Ctrl+D to quit) >>> ",fenf);
		if(!getline(cin,s)){puts("\n\nQuitting...");break;}
		if(s.empty())s="rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

		board b(s);
		for(int i=1;i<7;i++)b.ss={},printf("Perft %d = %10d,",i,b.perft(i))
			,printf(" %9d captures, %6d e.p's, %7d castles, %8d promotions, %8d checks.\n",b.ss.capt,b.ss.enp,b.ss.castle,b.ss.prom,b.ss.check)
			,fprintf(stderr,"Time used = %.3f\n",1.*clock()/CLOCKS_PER_SEC);
	}
}
