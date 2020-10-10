/********************************
 * 8x8 board
 * supported piece: King (K), Queen (Q), Rook (R), Bishop (B), Knight (N), Pawn (P)
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
inline int lowbit(mask s)
{
	static int index64[64]={
		 0,  1, 48,  2, 57, 49, 28,  3,
		61, 58, 50, 42, 38, 29, 17,  4,
		62, 55, 59, 36, 53, 51, 43, 22,
		45, 39, 33, 30, 24, 18, 12,  5,
		63, 47, 56, 27, 60, 41, 37, 16,
		54, 35, 52, 21, 44, 32, 23, 11,
		46, 26, 40, 15, 34, 20, 31, 10,
		25, 14, 19,  9, 13,  8,  7,  6
	};
	static mask x=0x03f79d71b4cb0a89;
	return index64[((s&-s)*x)>>58];
}
inline mask hibit(mask s){return 63-__builtin_clzll(s);}
inline bool ins(const string &s,char c){return s.find(c)!=string::npos;}

const int dx[]={-1,1,0,0,-1,1,-1,1},dy[]={0,0,-1,1,-1,1,1,-1},ndx[]={-2,2,-2,2,-1,1,-1,1},ndy[]={-1,1,1,-1,-2,2,2,-2};
mask Ratt[64][2],Batt[64][2];

struct backup
{
	mask x,*p;
	backup(mask *t=0):x(t?*t:0),p(t){}
	void restore(){*p=x;p=0;}
};

//$ sp(ecial move) - 0: normal, 1: O-O-O, 2: O-O, 3: =Q, 4: =R, 5: =B, 6: =N, 7: 2-space P jump (enables EnP)
struct play{mask from,to,capt,c,sp,pc;};
struct board
{
	vector<vector<mask>> castle;
	mask Plies,Move,toMove,enp,file[8],rank[8],X[2],K[2],Q[2],R[2],B[2],N[2],P[2],pstart[2],prom[2],all,Natt[64],Patt[2][64],Katt[64];
	vector<play> mlist;

	static mask encode(const string &s){return (s[1]-'1')*8+(s[0]-'a');}
	static string decode(mask x){return string()+char((x&7)+'a')+char((x>>3)+'1');}

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

	board(string s):file{},rank{},X{},K{},Q{},R{},B{},N{},P{},Natt{},Patt{},Katt{}
	{
		map<int,mask*> mp={{'K',K},{'Q',Q},{'R',R},{'B',B},{'N',N},{'P',P}};
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
		else enp=64;

		for(int i=0;i<8;i++)
			for(int j=0;j<8;j++)rank[i]^=1ull<<(i*8+j),file[i]^=1ull<<(j*8+i);

		pstart[0]=rank[0]|rank[1];
		pstart[1]=rank[6]|rank[7];
		prom[0]=rank[7];
		prom[1]=rank[0];
		maintain();

		for(int i=0;i<64;i++)
		{
			mask b=1ull<<i;
			Natt[i]= ((b&~file[6]&~file[7]&~rank[7])<<10)|((b&~file[6]&~file[7]&~rank[0])>>6)
					|((b&~file[0]&~file[1]&~rank[0])>>10)|((b&~file[0]&~file[1]&~rank[7])<<6)
					|((b&~rank[6]&~rank[7]&~file[7])<<17)|((b&~rank[6]&~rank[7]&~file[0])<<15)
					|((b&~rank[0]&~rank[1]&~file[0])>>17)|((b&~rank[0]&~rank[1]&~file[7])>>15);

			Katt[i]= ((b&~file[7])<<1)|((b&~file[0])>>1)
					|((b&~rank[0])>>8)|((b&~rank[7])<<8)
					|((b&~file[7]&~rank[7])<<9)|((b&~file[7]&~rank[0])>>7)
					|((b&~file[0]&~rank[7])<<7)|((b&~file[0]&~rank[0])>>9);

			Patt[0][i]=((b&~file[7])<<9)|((b&~file[0])<<7);
			Patt[1][i]=((b&~file[7])>>7)|((b&~file[0])>>9);
		}
	}

	mask KNPmove(mask c,mask k,mask *KNPatt,int type)
	{
		mask att=0;
		while(k)
		{
			int b=lowbit(k);
			att|=KNPatt[b];
			k^=1ull<<b;
		}
		return !type?att&~X[c]:att&X[!c];
	}
	mask Pmunch(mask c,mask p){return KNPmove(c,p,Patt[c],1);}
	mask Pmove(mask c,mask p){return !c
		?((p|(((p&pstart[0])<<8)&~all))<<8)&~all
		:((p|(((p&pstart[1])>>8)&~all))>>8)&~all;
	}
	mask Kmove(mask c,mask k){return KNPmove(c,k,Katt,0);}
	mask Nmove(mask c,mask n){return KNPmove(c,n,Natt,0);}

#define Xm(x,atk,piece)\
	for(mask pc=piece;pc;)\
	{\
		mask bit=lowbit(pc),p=1ull<<bit,att=atk[bit][x],obs=all&att,low=obs&(p-1),hi=obs^low;\
		low|=1;hi|=1ull<<63;\
		res|=(2*(1ull<<lowbit(hi))-(1ull<<hibit(low)))&att&~X[c];\
		pc^=p;\
	}\

	mask Rmove(mask c,mask r)
	{
		mask res=0;
		Xm(0,Ratt,r);
		Xm(1,Ratt,r);
		return res;
	}
	mask Bmove(mask c,mask b)
	{
		mask res=0;
		Xm(0,Batt,b);
		Xm(1,Batt,b);
		return res;
	}
	mask Qmove(mask c,mask q){return Rmove(c,q)|Bmove(c,q);}

	bool inCheck(mask c,mask p){return (Pmunch(!c,P[!c])|Kmove(!c,K[!c])|Nmove(!c,N[!c])|Rmove(!c,R[!c])|Bmove(!c,B[!c])|Qmove(!c,Q[!c]))&(1ull<<p);}
	void maintain(){X[0]=K[0]|Q[0]|R[0]|B[0]|N[0]|P[0];X[1]=K[1]|Q[1]|R[1]|B[1]|N[1]|P[1];all=X[0]|X[1];}

	backup bak[110001];
	int cnt=0,ptr[101],cur=0;
	void alloc(){ptr[++cnt]=cur;}
	void upd(mask &x,mask t){bak[++cur]=&x;x=t;}
	void revert(){while(cur!=ptr[cnt])bak[cur--].restore();cnt--;}
	void undo(){revert();maintain();}
	void confirm(){cur=ptr[--cnt];}

#define UPD(PC)\
	if(PC[p.c]&(1ull<<p.from))upd(PC[p.c],PC[p.c]^(1ull<<p.from)^(1ull<<p.to));\
	if(PC[!p.c]&(1ull<<p.capt))upd(PC[!p.c],PC[!p.c]^(1ull<<p.capt));

	bool trial(const play &p)
	{
		alloc();
		UPD(K);
		UPD(Q);
		UPD(R);
		UPD(B);
		UPD(N);
		UPD(P);
		maintain();
		bool f=!K[p.c]||!inCheck(p.c,lowbit(K[p.c]));
		undo();
		return f;
	}
	void add(mask from,mask to,mask capt,mask c,mask sp,mask pc){mlist.emplace_back(play{from,to,capt,c,sp,pc});}
	void getProm(mask c,mask from,mask to,mask capt)
	{
		if(trial({from,to,capt,c,0,5}))
		{
			if(~prom[c]&(1ull<<to))
			{
				add(from,to,capt,c,from-to==16||to-from==16?7:0,5);
				return;
			}
			add(from,to,capt,c,3,5);
			add(from,to,capt,c,4,5);
			add(from,to,capt,c,5,5);
			add(from,to,capt,c,6,5);
		}
	}

	const int begin[2][2]={{1,5},{57,61}},ending[2][2]={{3,6},{59,62}};
	mask getMoves(mask c)
	{
		mlist.clear();
		//% castling, 0: O-O-O, 1: O-O
		for(int d=0;d<2;d++)
			if(castle[c][d])
			{
				int sq[]={!c?4:60,d?!c?5:61:!c?3:59,d?!c?6:62:!c?2:58},beg=begin[c][d],end=ending[c][d];
				if(inCheck(c,sq[0])||inCheck(c,sq[1])||inCheck(c,sq[2]))continue;
				if(all&((1ull<<(end+1))-(1ull<<beg)))continue;
				add(64,64,64,c,d+1,0);
			}
		//* En Passant
		if((enp>>3)==2&&c)
		{
			if((P[c]&(1ull<<(enp+7)))&&(enp&7)>0)getProm(c,enp+7,enp,enp+8);
			if((P[c]&(1ull<<(enp+9)))&&(enp&7)<7)getProm(c,enp+9,enp,enp+8);
		}
		if((enp>>3)==5&&!c)
		{
			if((P[c]&(1ull<<(enp-9)))&&(enp&7)>0)getProm(c,enp-9,enp,enp-8);
			if((P[c]&(1ull<<(enp-7)))&&(enp&7)<7)getProm(c,enp-7,enp,enp-8);
		}
		//` Pawn moves
		for(mask m=P[c];m;)
		{
			mask b=lowbit(m),sq[]={!c?b+16:b-16,!c?b+8:b-8,!c?b+7:b-9,!c?b+9:b-7};
			if((pstart[c]&(1ull<<b))&&(~all&(1ull<<sq[0]))&&(~all&(1ull<<sq[1])))getProm(c,b,sq[0],64);
			if(~all&(1ull<<sq[1]))getProm(c,b,sq[1],64);
			if((X[!c]&(1ull<<sq[2]))&&(b&7)>0)getProm(c,b,sq[2],sq[2]);
			if((X[!c]&(1ull<<sq[3]))&&(b&7)<7)getProm(c,b,sq[3],sq[3]);
			m^=1ull<<b;
		}
		//? Other pieces
#define ADD(pieces,move,id,pc)\
	for(mask m=pieces[c];m;)\
	{\
		mask b=lowbit(m),q=move(c,1ull<<b);\
		while(q)\
		{\
			mask b2=lowbit(q);\
			play go={b,b2,all&(1ull<<b2)?b2:64,c,0,pc};\
			if(trial(go))mlist.emplace_back(go);\
			q^=1ull<<b2;\
		}\
		m^=1ull<<b;\
	}

		ADD(N,Nmove,"N",4);
		ADD(K,Kmove,"K",0);
		ADD(R,Rmove,"R",2);
		ADD(B,Bmove,"B",3);
		ADD(Q,Qmove,"Q",1);

		return mlist.size();
	}

	void move(const play &p)
	{
		alloc();
		const mask &from=p.from,&to=p.to,&capt=p.capt,&c=p.c,&sp=p.sp,&pc=p.pc;

		if(sp==1||sp==2) //! Castling
		{
			mask type=sp-1,bk=!c?4:60,ek=!type?bk-2:bk+2,br=!c?!type?0:7:!type?56:63,er=!type?ek+1:ek-1;
			upd(K[c],K[c]^(1ull<<bk)^(1ull<<ek));
			upd(R[c],R[c]^(1ull<<br)^(1ull<<er));
		}
		else if(3<=sp&&sp<=6) //? Promotion
		{
			upd(P[c],P[c]^(1ull<<from));
			mask &go=(sp==3?Q:sp==4?R:sp==5?B:N)[c];
			upd(go,go^(1ull<<to));
		}
		else if(sp==7)upd(P[c],P[c]^(1ull<<from)^(1ull<<to));//^ Pawn Double Jump
		else //` Normal move
		{
			mask &go=(!pc?K:pc==1?Q:pc==2?R:pc==3?B:pc==4?N:P)[c];
			upd(go,go^(1ull<<from)^(1ull<<to));
		}

		if(capt!=64)
		{
			if(K[!c]&(1ull<<capt))upd(K[!c],K[!c]^(1ull<<capt));
			if(Q[!c]&(1ull<<capt))upd(Q[!c],Q[!c]^(1ull<<capt));
			if(R[!c]&(1ull<<capt))upd(R[!c],R[!c]^(1ull<<capt));
			if(B[!c]&(1ull<<capt))upd(B[!c],B[!c]^(1ull<<capt));
			if(N[!c]&(1ull<<capt))upd(N[!c],N[!c]^(1ull<<capt));
			if(P[!c]&(1ull<<capt))upd(P[!c],P[!c]^(1ull<<capt));
		}

		upd(Plies,pc==5||capt!=64?0:Plies+1);
		upd(enp,sp==7?(from+to)>>1:64);
		if(!pc||(pc==2&&((!c&&from==0)||(c&&from==56)))||sp==1||sp==2)upd(castle[c][0],0);
		if(!pc||(pc==2&&((!c&&from==7)||(c&&from==63)))||sp==1||sp==2)upd(castle[c][1],0);
		if((!c&&capt==56)||(c&&capt==0))upd(castle[!c][0],0);
		if((!c&&capt==63)||(c&&capt==7))upd(castle[!c][1],0);
		upd(toMove,toMove^1);
		if(c)upd(Move,Move+1);
		maintain();
	}

	int perft(int d)
	{
		if(!d)return 1;
		getMoves(toMove);
		int ans=0;
		auto MoveList=mlist;
		for(auto &x:MoveList)
		{
			move(x);
			ans+=perft(d-1);
			undo();
		}
		return ans;
	}
};

const char *fenf="Fen Syntax:\n\t<board (rank-8'/'...'/'rank-2'/'rank-1)> <side-to-move ('w'|'b')> \
<castling ('-'|['K']['Q']['k']['q'])> <EnP square ('-'|square)> <Plies (#)> <Moves (#)>\n\t\
White - Uppercase, Black - Lowercase, # - Number of consecutive empty square\n\t\
e.g. 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'";

int main()
{
	//! Rook, Bishop, Queen Preprocess
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
		printf("%s\n\nFen (enter nothing to get default) >>> ",fenf);
		getline(cin,s);
		if(s.empty())s="rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

		board b(s),bbak(s);
		for(int i=1;i<6;i++)printf("Perft %d = %d\n",i,b.perft(i));

		printf("Rematch? (Y/N) > ");
		getline(cin,s);
		if(!s.empty()&&toupper(s[0])=='N')break;
	}
}
