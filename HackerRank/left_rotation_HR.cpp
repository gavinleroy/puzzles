#include <cstdio>
#include <vector>

using namespace std;

int main(void){
	int n, d;
	scanf("%d %d", &n, &d);
	vector<int> vv;
	for(int i = 1; i <= d; i++){
		int t;
		scanf("%d", &t);
		vv.push_back(t);	
	}
	for(int i = 0; i < (n - d); i++){
		int t;
		scanf("%d", &t);
		printf("%d ", t);
	}
	for(int i = 0; i < vv.size(); i++){
		printf("%d ", vv[i]);
	}
	printf("\n");
	return 0;
}
