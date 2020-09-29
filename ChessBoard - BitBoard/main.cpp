/********************************
 * 8x8 board
 * supported piece: King (K), Queen (Q), Rook (R), Bishop (B), Knight (N), Pawn (P)
 * * *--*--*--*--*--*--*--*--*
 * * |56|57|58|59|60|61|62|63| 8
 * * *--*--*--*--*--*--*--*--*
 * * |48|49|50|51|52|53|54|55| 7
 * * *--*--*--*--*--*--*--*--*
 * * |40|41|42|43|44|45|46|47| 6
 * * *--*--*--*--*--*--*--*--*
 * * |32|33|34|35|36|37|38|39| 5
 * *--*--*--*--*--*--*--*--*
 * * |24|25|26|27|28|29|30|31| 4
 * * *--*--*--*--*--*--*--*--*
 * * |16|17|18|19|20|21|22|23| 3
 * * *--*--*--*--*--*--*--*--*
 * * | 8| 9|10|11|12|13|14|15| 2
 * * *--*--*--*--*--*--*--*--*
 * * | 0| 1| 2| 3| 4| 5| 6| 7| 1
 * * *--*--*--*--*--*--*--*--*
 * *   a  b  c  d  e  f  g  h
 * * file a-h, rank 1-8
********************************/

#include<iostream>
#include<cctype>
#include<vector>
#include<sstream>
#include<map>
#include<algorithm>
#include<cstring>
#include<cassert>
#define popcnt __builtin_popcountll
using namespace std;

using mask=unsigned long long;
inline int lowbit(mask s){return __builtin_ffsll(s)-1;}
inline int hibit(mask s){return 63-__builtin_clzll(s);}
inline bool ins(const string &s,char c){return s.find(c)!=string::npos;}

const int dx[]={-1,1,0,0,-1,1,-1,1},dy[]={0,0,-1,1,-1,1,1,-1},ndx[]={-2,2,-2,2,-1,1,-1,1},ndy[]={-1,1,1,-1,-2,2,2,-2};
mask Ratt[64][2],Batt[64][2];

struct backup
{
	mask x,*p;
	backup(mask &t):x(t),p(&t){}
	void restore(){*p=x;p=0;}
};
vector<backup> bak;
void upd(mask &x,mask t){bak.emplace_back(x);x=t;}
void revert(){for(auto &x:bak)x.restore();bak.clear();}

//$ sp(ecial move) - 0: normal, 1: O-O-O, 2: O-O, 3: =Q, 4: =R, 5: =B, 6: =N, 7: 2-space P jump (enables EnP)
struct play{int from,to,capt,c,sp,pc;};
struct board
{
	int Plies,Move,toMove,enp;
	vector<vector<int>> castle;
	mask file[8],rank[8],X[2],K[2],Q[2],R[2],B[2],N[2],P[2],pstart[2],prom[2];
	multimap<string,play> choice;

	static int encode(const string &s){return (s[1]-'1')*8+(s[0]-'a');}
	static string decode(int x){return string()+char((x&7)+'a')+char((x>>3)+'1');}

	static void printbb(mask bb,string info)
	{
		cout<<endl<<info<<" = \n";
		for(int i=7;i>=0;i--)
		{
			for(int j=0;j<8;j++)printf("%d",bool(bb&(1ull<<(i*8+j))));
			puts("/");
		}
		puts("");
	}

	board(string s):file{},rank{},X{},K{},Q{},R{},B{},N{},P{},pstart{},prom{}
	{
		map<char,mask*> mp={{'K',K},{'Q',Q},{'R',R},{'B',B},{'N',N},{'P',P}};
		for(char &c:s)if(c=='/')c=' ';
		string t;
		istringstream in(s);
		for(int i=7;i>=0;i--)
		{
			in>>t;
			for(int j=0,k=0;j<8;k++)
				if(isdigit(t[k]))j+=t[k]-'0';
				else mp[toupper(t[k])][islower(t[k])]^=1ull<<(i*8+j++);
		}

		in>>t;
		toMove=(t[0]=='b');
		in>>t;
		castle={{ins(t,'Q'),ins(t,'K')},{ins(t,'q'),ins(t,'k')}};
		in>>t>>Plies>>Move;
		if(t!="-")enp=encode(t);
		else enp=-1;
		maintain();

		for(int i=0;i<8;i++)
			for(int j=0;j<8;j++)rank[i]^=1ull<<(i*8+j),file[i]^=1ull<<(j*8+i);
		bak.reserve(8);
		pstart[0]=rank[0]|rank[1];
		pstart[1]=rank[6]|rank[7];
		prom[0]=rank[7];
		prom[1]=rank[0];
	}

	mask all(){return X[0]^X[1];}

	mask Pmove(int c,mask p){return !c
		?((p|(((p&pstart[0])<<8)&~all()))<<8)&~all()
		:((p|(((p&pstart[1])>>8)&~all()))>>8)&~all();
	}
	mask Pmunch(int c,mask p){return !c
		?(((p&~file[7])<<9)|((p&~file[0])<<7))&X[1]
		:(((p&~file[7])>>7)|((p&~file[0])>>9))&X[0];
	}

	mask Kmove(int c,mask k){return
		(((k&~file[7])<<1)|((k&~file[0])>>1)
		|((k&~rank[0])>>8)|((k&~rank[7])<<8)
		|((k&~file[7]&~rank[7])<<9)|((k&~file[7]&~rank[0])>>7)
		|((k&~file[0]&~rank[7])<<7)|((k&~file[0]&~rank[0])>>9))&~X[c];
	}
	mask Nmove(int c,mask n){return
		(((n&~file[6]&~file[7]&~rank[7])<<10)|((n&~file[6]&~file[7]&~rank[0])>>6)
		|((n&~file[0]&~file[1]&~rank[0])>>10)|((n&~file[0]&~file[1]&~rank[7])<<6)
		|((n&~rank[6]&~rank[7]&~file[7])<<17)|((n&~rank[6]&~rank[7]&~file[0])<<15)
		|((n&~rank[0]&~rank[1]&~file[0])>>17)|((n&~rank[0]&~rank[1]&~file[7])>>15))&~X[c];
	}

#define Xm(x,atk,piece)\
	for(mask pc=piece;pc;)\
	{\
		int bit=lowbit(pc);\
		mask p=1ull<<bit,att=atk[bit][x],obs=all()&att,low=obs&(p-1),hi=obs^low;\
		low|=1;hi|=1ull<<63;\
		res|=(2*(1ull<<lowbit(hi))-(1ull<<hibit(low)))&att&~X[c];\
		pc^=p;\
	}\

	mask Rmove(int c,mask r)
	{
		mask res=0;
		Xm(0,Ratt,r);
		Xm(1,Ratt,r);
		return res;
	}
	mask Bmove(int c,mask b)
	{
		mask res=0;
		Xm(0,Batt,b);
		Xm(1,Batt,b);
		return res;
	}
	mask Qmove(int c,mask q)
	{
		mask res=0;
		Xm(0,Ratt,q);
		Xm(1,Ratt,q);
		Xm(0,Batt,q);
		Xm(1,Batt,q);
		return res;
	}

	bool inCheck(int c,int p){return (Pmunch(!c,P[!c])|Kmove(!c,K[!c])|Nmove(!c,N[!c])|Rmove(!c,R[!c])|Bmove(!c,B[!c])|Qmove(!c,Q[!c]))&(1ull<<p);}
	void maintain(){X[0]=K[0]|Q[0]|R[0]|B[0]|N[0]|P[0];X[1]=K[1]|Q[1]|R[1]|B[1]|N[1]|P[1];}

	const int begin[2][2]={{1,5},{57,61}},ending[2][2]={{3,6},{59,62}},Rpos[2][2]={{0,7},{56,63}},Kpos[2]={4,60}
		,Rend[2][2]={{3,5},{59,61}},Kend[2][2]={{2,6},{58,62}};
	void undo(){revert();maintain();}
	bool trial(const play &p)
	{
		if(p.sp==7)upd(P[p.c],P[p.c]^(1ull<<p.from)^(1ull<<p.to));
		else
		{
			mask *x[]={K,Q,R,B,N,P};
			for(int i=0;i<6;i++)
			{
				if(x[i][p.c]&(1ull<<p.from))upd(x[i][p.c],x[i][p.c]^(1ull<<p.from)^(1ull<<p.to));
				if(x[i][!p.c]&(1ull<<p.capt))upd(x[i][!p.c],x[i][!p.c]^(1ull<<p.capt));
			}
		}
		maintain();
		bool f=!inCheck(p.c,lowbit(K[p.c]));
		undo();
		return f;
	}
	void add(const string &s,int from,int to,int capt,int c,int sp,int pc){choice.emplace(s,play{from,to,capt,c,sp,pc});}
	void getProm(int c,int from,int to,int capt)
	{
		if(trial({from,to,capt,c,0,5}))
		{
			string s;
			if(capt!=64)s+=decode(from)[0],s+='x';
			s+=decode(to);
			if(~prom[c]&(1ull<<to))
			{
				add(s,from,to,capt,c,abs(from-to)==16?7:0,5);
				return;
			}
			add(s+"=Q",from,to,capt,c,3,5);
			add(s+"=R",from,to,capt,c,4,5);
			add(s+"=B",from,to,capt,c,5,5);
			add(s+"=N",from,to,capt,c,6,5);
		}
	}
	void getMoves(int c)
	{
		choice.clear();
		//% castling, 0: O-O-O, 1: O-O
		for(int d=0;d<2;d++)
			if(castle[c][d])
			{
				int sq[]={!c?4:60,d?!c?5:61:!c?3:59,d?!c?6:62:!c?2:58},beg=begin[c][d],end=ending[c][d];
				if(!castle[c][d]||inCheck(c,sq[0])||inCheck(c,sq[1])||inCheck(c,sq[2]))continue;
				if(all()&((1ull<<(end+1))-(1ull<<beg)))continue;
				add(d?"O-O":"O-O-O",64,64,64,c,d+1,6);
			}
		//* En Passant
		if(enp!=-1&&(P[c]&(1ull<<(enp-1)))&&(enp&7))getProm(c,enp-1,!c?enp+8:enp-8,enp);
		if(enp!=-1&&(P[c]&(1ull<<(enp+1)))&&(enp&7)<7)getProm(c,enp+1,!c?enp+8:enp-8,enp);
		//` Pawn moves
		for(mask m=P[c];m;)
		{
			int b=lowbit(m),sq[]={!c?b+16:b-16,!c?b+8:b-8,!c?b+7:b-9,!c?b+9:b-7};
			if((pstart[c]&(1ull<<b))&&(~all()&(1ull<<sq[0]))&&(~all()&(1ull<<sq[1])))getProm(c,b,sq[0],64);
			if(~all()&(1ull<<sq[1]))getProm(c,b,sq[1],64);
			if((X[!c]&(1ull<<sq[2]))&&(b&7))getProm(c,b,sq[2],sq[2]);
			if((X[!c]&(1ull<<sq[3]))&&((b&7)<7))getProm(c,b,sq[3],sq[3]);
			m^=1ull<<b;
		}
		//? Other pieces
#define ADD(pieces,move,id,pc)\
	for(mask m=pieces[c];m;)\
	{\
		int b=lowbit(m);\
		mask q=move(c,1ull<<b);\
		while(q)\
		{\
			int b2=lowbit(q);\
			play go={b,b2,all()&(1ull<<b2)?b2:64,c,0,pc};\
			if(trial(go))choice.emplace(string(id)+(all()&(1ull<<b2)?"X":"")+decode(b2),go);\
			q^=1ull<<b2;\
		}\
		m^=1ull<<b;\
	}

		ADD(N,Nmove,"N",4);
		ADD(K,Kmove,"K",0);
		ADD(R,Rmove,"R",2);
		ADD(B,Bmove,"B",3);
		ADD(Q,Qmove,"Q",1);

		multimap<string,play> New;
		int attr[65][9][7],att[65][7];
		memset(attr,0,sizeof attr);
		memset(att,0,sizeof att);
		for(auto &[x,y]:choice)
			if(y.to!=64)attr[y.to][y.from>>3][y.pc]++,att[y.to][y.pc]++;
		for(auto &[x,y]:choice)
			if(att[y.to][y.pc]>1)assert(isupper(x[0])),New.emplace(x.substr(0,1)+(attr[y.to][y.from>>3][y.pc]>1?char((y.from&7)+'a'):char((y.from>>3)+'0'))+x.substr(1),y);
			else New.emplace(x,y);
		choice=New;
	}

	void move(const string &s);
};

const char *fenf="Fen Syntax:\n\t<board (rank-8'/'...'/'rank-2'/'rank-1)> <side-to-move ('w'|'b')> \
<castling ('-'|['K']['Q']['k']['q'])> <EnP square ('-'|square)> <Plies (#)> <Moves (#)>\n\t\
White - Uppercase, Black - Lowercase, # - Number of consecutive empty square\n\t\
e.g. 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'";
int main()
{
	for(int r=0;r<8;r++)
		for(int f=0;f<8;f++)
			for(int d=0;d<2;d++)
			{
				for(int q=2*d;q<2*d+2;q++)
					for(int i=1;i<8;i++)
					{
						int x=r+i*dx[q],y=f+i*dy[q];
						if(x<0||y<0||x>7||y>7)break;
						Ratt[r*8+f][d]|=1ull<<(x*8+y);
					}
				for(int q=2*d+4;q<2*d+6;q++)
					for(int i=1;i<8;i++)
					{
						int x=r+i*dx[q],y=f+i*dy[q];
						if(x<0||y<0||x>7||y>7)break;
						Batt[r*8+f][d]|=1ull<<(x*8+y);
					}
			}
	for(string s;;)
	{
		system("clear");
		printf("Use default starting position? (Y/N) > ");
		cin>>s;
		if(toupper(s[0])=='Y')s="rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
		else
		{
			printf("%s\n\nFen >>> ",fenf);
			getline(cin>>ws,s);
		}
		board b(s);
#if 0 //$ Not done yet...
		for(int &c=b.toMove;;c^=1)
		{
			//@ play the fking game
		}
#else //` just print all legal moves lol, ignoring the pitiful '+' '#'
		b.getMoves(b.toMove);
		for(auto &[x,y]:b.choice)printf("%s: %s -> %s, capt %s, special = %d, piece #%d\n",x.c_str(),b.decode(y.from).c_str()
			,b.decode(y.to).c_str(),y.capt==64?"none":b.decode(y.capt).c_str(),y.sp,y.pc);
#endif
		printf("Rematch? (Y/N) > ");
		cin>>s;
		if(toupper(s[0])=='N')break;
	}
}
