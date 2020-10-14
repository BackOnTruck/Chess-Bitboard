// perft test program
#include "board.hpp"
int main()
{
	puts("Running...");
	board b("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
	printf("Perft 6 = %d\n",b.perft(6));
	fprintf(stderr,"Time used = %.3f\n\n",1.*clock()/CLOCKS_PER_SEC);
}
