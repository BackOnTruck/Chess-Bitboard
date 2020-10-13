// used for playing
#include "board.hpp"

int main()
{
	for(string s;;)
	{
		printf("%s\n\nFen (Enter to get default, Ctrl+D to quit) >>> ",fenf);
		if(!getline(cin,s)){puts("\n\nQuitting...");break;}
		if(s.empty())s="rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

		board b(s);
		map<mask,int> pos;
		for(auto &c=b.toMove;;)
		{
			for(int i=0;i<64;i++)
			{
				char &ch=t[i>>3][i&7];
				ch='.';

				if(b.K[0]&(1ull<<i))ch='K';
				if(b.Q[0]&(1ull<<i))ch='Q';
				if(b.R[0]&(1ull<<i))ch='R';
				if(b.B[0]&(1ull<<i))ch='B';
				if(b.N[0]&(1ull<<i))ch='N';
				if(b.P[0]&(1ull<<i))ch='P';

				if(b.K[1]&(1ull<<i))ch='k';
				if(b.Q[1]&(1ull<<i))ch='q';
				if(b.R[1]&(1ull<<i))ch='r';
				if(b.B[1]&(1ull<<i))ch='b';
				if(b.N[1]&(1ull<<i))ch='n';
				if(b.P[1]&(1ull<<i))ch='p';
			}
			pprint();

			mask mv=b.getMoves(c),chk=b.inCheck(c,lowbit(b.K[c]));
			if(chk&&!mv){printf("Checkmate. %llu-%d\n",c,!c);break;}

			if(!chk&&!mv){puts("Stalemate. 1/2-1/2");break;}
			if(++pos[b.hash()]>2){puts("3-fold repetition. 1/2-1/2");break;}
			if(b.Plies>99){puts("50-move rule. 1/2-1/2");break;}
			if(!popcnt(b.Q[0])&&!popcnt(b.R[0])&&!popcnt(b.P[0])&&popcnt(b.N[0])+popcnt(b.B[0])<2
				&&!popcnt(b.Q[1])&&!popcnt(b.R[1])&&!popcnt(b.P[1])&&popcnt(b.N[1])+popcnt(b.B[1])<2){puts("Insufficient material. 1/2-1/2");break;}

			multimap<string,play> moves,New;
			for(auto &p:b.mlist)
			{
				mask from=p.from,to=p.to,capt=p.capt,sp=p.sp,pc=p.pc;
				s="";
				if(sp==1)s="O-O-O";
				else if(sp==2)s="O-O";
				else if(pc<5)
				{
					s=!pc?'K':pc==1?'Q':pc==2?'R':pc==3?'B':'N';
					if(capt!=64)s+='x';
					s+=decode(to);
				}
				else
				{
					if(capt==64)s=decode(to);
					else s=decode(from)[0],s+='x'+decode(to);
					if(sp==3)s+="=Q";
					else if(sp==4)s+="=R";
					else if(sp==5)s+="=B";
					else if(sp==6)s+="=N";
				}
				moves.insert({s,p});
			}

			int attf[65][9][7],att[65][7];
			memset(attf,0,sizeof attf);
			memset(att,0,sizeof att);
			for(auto &[x,y]:moves)
				if(y.to!=64&&y.pc<5)attf[y.to][y.from&7][y.pc]++,att[y.to][y.pc]++;
			for(auto &[x,y]:moves)
				if(att[y.to][y.pc]>1&&y.pc<5)New.emplace(x.substr(0,1)+(attf[y.to][y.from&7][y.pc]>1?char((y.from>>3)+'1'):char((y.from&7)+'a'))+x.substr(1),y);
				else New.emplace(x,y);
			moves=New;
			for(printf("%s to move. Enter move: ",c?"Black":"White");;)
			{
				getline(cin,s);
				if(moves.count(s))break;
				puts("Invalid move. Here're all legal moves.");
				for(auto &[x,y]:moves)cout<<x<<' ';
				printf("\nTry again: ");
			}
			b.move(moves.lower_bound(s)->second);
		}
	}
}
hite - Uppercase, Black - Lowercase, # - Number of consecutive empty square\n\t\
e.g. 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'";

int main()
{
	for(string s;;)
	{
		printf("%s\n\nFen (Enter to get default, Ctrl+D to quit) >>> ",fenf);
		if(!getline(cin,s)){puts("\n\nQuitting...");break;}
		if(s.empty())s="rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

		board b(s);
		for(int i=1;i<6;i++)b.ss={},printf("Perft %d = %10d,",i,b.perft(i)),printf(" %9d captures, %6d e.p's, %7d castles, %8d promotions, %8d checks.\n",b.ss.capt,b.ss.enp,b.ss.castle,b.ss.prom,b.ss.check);
	}
}
