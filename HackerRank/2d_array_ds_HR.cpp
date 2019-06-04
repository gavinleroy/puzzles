#include <cstdio>
#define MIN_N (1 << 31)

using namespace std;

int main(void){
	int** aa = new int*[6];

	for(int i = 0; i < 6; i++){
		aa[i] = new int[6];
		for(int j = 0; j < 6; j++){
			scanf("%d", &aa[i][j]);
		}
	}
	int max = MIN_N; 
	for(int i = 0; i < 4; i++){
		for(int j = 2; j < 6; j++){
			int t = aa[i][j] + aa[i][j-1] + aa[i][j-2] + aa[i+1][j-1] + aa[i+2][j] + aa[i+2][j-1] + aa[i+2][j-2];
			max = ((t > max) ? t: max);
		}
	}
	printf("%d\n", max);
	return 0;
}
