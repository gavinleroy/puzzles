#include <iostream>
#include <cmath>
#include <string>
#include <cstring>
#include <utility>
#include <algorithm>
#include <vector>
#include <stack>
#include <cassert>
#define NL '\n'
#define PII pair<int,int>
#define VPII vector<PII >
#define MP(x,y) make_pair(x,y)
#define F first
#define S second
using namespace std;

#define probe(ss,i) cout<<i<<endl; fflush(stdout); cin>>t; ss+=t; 	\
		    if(t == "E") return;	        		//\

//		    cout<<i<<endl; fflush(stdout); cin>>t; ss+=t;   	\
//		    cout<<i<<endl; fflush(stdout); cin>>t; ss+=t;

void solve(int l, int r){
	string ans,t;
	bool gh = true, lied=false;
	ans=t="";
	while(1){
		int m=(l+r)/2;
		if(!lied) m = gh ? r : l;
		probe(ans, m);
	}
}

int main(){
#ifdef LOCAL
	freopen("input.1", "r", stdin);
#endif
	int N; cin>>N;
	solve(1, N);
	return 0;
}
