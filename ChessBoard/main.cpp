/***********************************************
 *
 * Simple chess board CLI application
 * Does not support en passant / castling
 * Pawns promote to Queens
 *
***********************************************/
#include<bits/stdc++.h>
#define x first
#define y second
#define DoPieceMove(low,up,lim,xdx,ydy,chk)\
	for(int d=low;d<up;d++)\
		for(int p=1;p<=lim;p++)\
		{\
			int x=i+p*xdx[d],y=j+p*ydy[d];\
			if(!x||!y||x>n||y>m||c==pType(x,y)||c=='#')break;\
			if(chk)\
			{\
				if(trial(i,j,x,y,c))moves[i][j].emplace(x,y);\
				if(pType(x,y)==!c)break;\
			}\
			else if(pType(x,y)==!c)\
			{\
				if(tolower(s[x][y])=='k')f=1;\
				break;\
			}\
		}
using namespace std;
const int dx[]={-1,0,0,1,-1,-1,1,1},dy[]={0,-1,1,0,-1,1,-1,1},ndx[]={-2,-2,-1,-1,1,1,2,2},ndy[]={-1,1,-2,2,-2,2,-1,1};
using coord=pair<int,int>;
class board
{
	char s[20][20],t[20][20];
	set<coord> moves[20][20];
public:
	int n,m;
	board()
	{
		printf("Input board size (nxm; at most 16x16): ");
		scanf("%dx%d",&n,&m);
		puts("Input board (White lowercase, Black uppercase, e.g. k.K..Q.q)");
		for(int i=n;i;i--)scanf("%s",s[i]+1);
	}
	int pType(int x,int y){return islower(s[x][y])?0:isupper(s[x][y])?1:-1;}
	bool kingCaptured(int c)
	{
		for(int i=1;i<=n;i++)
			for(int j=1;j<=m;j++)
				if(pType(i,j)==c&&tolower(s[i][j])=='k')return 0;
		return 1;
	}
	bool inCheck(int c)
	{
		bool f=0;
		c=!c;
		for(int i=1;i<=n;i++)
			for(int j=1;j<=m;j++)
			{
				if(pType(i,j)!=c)continue;
				char ch=tolower(s[i][j]);
				if(ch=='k')DoPieceMove(0,8,1,dx,dy,0)
				else if(ch=='n')DoPieceMove(0,8,1,ndx,ndy,0)
				else if(ch=='r')DoPieceMove(0,4,20,dx,dy,0)
				else if(ch=='b')DoPieceMove(4,8,20,dx,dy,0)
				else if(ch=='q')DoPieceMove(0,8,20,dx,dy,0)
				else if(ch=='p')f|=(!c&&(s[i+1][j-1]=='K'||s[i+1][j+1]=='K'))||(c&&(s[i-1][j-1]=='k'||s[i-1][j+1]=='k'));
				else throw "Piece not supported";
			}
		return f;
	}
	bool trial(int sx,int sy,int tx,int ty,int c)
	{
		if(pType(sx,sy)==pType(tx,ty))return 0;
		move(sx,sy,tx,ty);
		bool f=inCheck(c);
		revertMove();
		return !f;
	}
	bool getValidMove(int c)
	{
		bool f=0;
		for(int i=1;i<=n;i++)
			for(int j=1;j<=m;j++)
			{
				moves[i][j].clear();
				if(pType(i,j)!=c)continue;
				char ch=tolower(s[i][j]);
				if(ch=='k')DoPieceMove(0,8,1,dx,dy,1)
				else if(ch=='n')DoPieceMove(0,8,1,ndx,ndy,1)
				else if(ch=='r')DoPieceMove(0,4,20,dx,dy,1)
				else if(ch=='b')DoPieceMove(4,8,20,dx,dy,1)
				else if(ch=='q')DoPieceMove(0,8,20,dx,dy,1)
				else if(ch=='p')
				{
					int x=c?i-1:i+1,x2=c?i-2:i+2;
					if(x>=1&&x<=n&&s[x][j]=='.'&&trial(i,j,x,j,c))moves[i][j].emplace(x,j);
					if(x2>=1&&x2<=n&&s[x][j]=='.'&&s[x2][j]=='.'&&(c?i==n-1:i==2)&&trial(i,j,x2,j,c))moves[i][j].emplace(x2,j);
					for(int y=j-1;y<j+2;y+=2)
						if(y>=1&&y<=m&&pType(x,y)==!pType(i,j)&&trial(i,j,x,y,c))moves[i][j].emplace(x,y);
				}
				else throw "Piece not supported";
				if(!moves[i][j].empty())f=1;
			}
		return f;
	}
	void move(int sx,int sy,int tx,int ty)
	{
		memcpy(t,s,sizeof s);
		s[tx][ty]=s[sx][sy];
		s[sx][sy]='.';
		if(s[tx][ty]=='p'&&tx==n)s[tx][ty]='q';
		if(s[tx][ty]=='P'&&tx==1)s[tx][ty]='Q';
	}
	void revertMove(){memcpy(s,t,sizeof s);}
	void draw()
	{
		printf("  *");
		for(int i=1;i<=m;i++)printf("-*");
		puts("");
	}
	void pprint()
	{
		draw();
		for(int i=n;i;i--)
		{
			printf("%2d|",i);
			for(int j=1;j<=m;j++)printf("%c|",s[i][j]);
			puts("");
			draw();
		}
		printf("  ");
		for(int i=1;i<=m;i++)printf(" %c",'a'+i-1);
		puts("");
	}
	bool valid(int sx,int sy,int tx,int ty){return moves[sx][sy].count({tx,ty});}
	void printMove(int x,int y)
	{
		if(moves[x][y].empty())return;
		printf("%lld moves from %c%d -> ",moves[x][y].size(),y+'a'-1,x);
		for(auto p:moves[x][y])printf("%c%d ",p.y+'a'-1,p.x);
		puts("");
	}
}b;
int main()
{
	for(int i=0;;i++)
	{
		system("cls");
		b.pprint();
		if(!b.getValidMove(i&1))
		{
			puts(!b.inCheck(i&1)&&!b.kingCaptured(i&1)?"Stalemate. 1/2-1/2":i&1?"Black Checkmated. 1-0":"White Checkmated. 0-1");
			return 0;
		}
		for(;;)
		{
			char s[9],c1,c2;
			printf("Enter %s move (e.g. e2-e4): ",i&1?"Black":"White");
			scanf("%s",s);
			int sx,sy,tx,ty;
			sscanf(s,"%c%d-%c%d",&c1,&sx,&c2,&tx);
			sy=c1-'a'+1;
			ty=c2-'a'+1;
			if(b.valid(sx,sy,tx,ty)){b.move(sx,sy,tx,ty);break;}
			printf("Invalid move. List all moves? [y/n]: ");
			scanf("%s",s);
			if(s[0]=='y')
				for(int r=1;r<=b.n;r++)
					for(int c=1;c<=b.m;c++)b.printMove(r,c);
		}
	}
}
