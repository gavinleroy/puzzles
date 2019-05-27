#include <cstdio>
#include <algorithm>
#define MAX_N 300001

typedef long long ll;

using namespace std;

struct node{
	int k_miss;
	int s;
	int e;
	int i;
	int p;
};

auto cmp = [](const node a, const node b){ return a.s < b.s; };

int main(void){
	node* gg = new node[MAX_N];
	int* aa = new int[MAX_N];
	int n;
	scanf("%d", &n);
	for(int i = 1; i <= n; i++){
		scanf("%d", &gg[i].s);
		gg[i].i = i;
	}
	for(int i = 1; i <= n; i++){
		scanf("%d", &gg[i].e);
		gg[i].k_miss = gg[i].e - gg[i].s - 1;
	}
	sort(gg+1, gg+n+1, cmp);
	aa[gg[1].i] = 0;
	gg[1].p = 0;
	node* p = gg + 1;
	for(int i = 2; i <= n; i++){
		aa[gg[i].i] = p->i;	
		gg[i].p = p->s+1;
		p->k_miss--;
		if(gg[i].k_miss > 0) p = gg+i;
		else{
			while(p->k_miss == 0 && p->s != 0){
				gg[p->p].k_miss -= p->e - p->s - 1;
				p = gg+p->p;
			}		
		}
	}	
	for(int i = 1; i <= n; i++) printf("%d ", aa[i]);
	printf("\n");
	delete []gg;
	delete []aa;
	return 0;
}
