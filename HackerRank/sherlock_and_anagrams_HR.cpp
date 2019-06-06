#include <iostream>
#include <string>
#include <utility>
#include <vector>
#define BOOST std::ios::sync_with_stdio(0); cin.tie(NULL); cout.tie(NULL);
#define P std::pair<int, int>
#define F first
#define S second

using namespace std;

string s = "";
int* aa = new int[26];

bool is_anagram(int a1, int a2, int b1, int b2){
	bool ret = true;
	for(int i = a1, j = b1; i <= a2 && j <= b2; i++, j++){
		aa[s[i] - 97]++;
		aa[s[j] - 97]--;
	}
	for(int i = 0; i < 26; i++){
		if(aa[i] != 0){
			ret = false;
			aa[i] = 0;
		}
	}
	return ret;
}

int main(void){BOOST
	int q;
	cin >> q;
	for(int i = 0; i < 26; i++) aa[i] = 0;
	while(q--){
		int ans = 0;
		cin >> s;
		vector<P> vv[101];
		for(int i = 0; i < s.size(); i++){
			for(int j = i; j < s.size(); j++){
				vv[j-i].push_back(make_pair(i, j));
			}
		}
		for(int i = 0; i < 101; i++){
			for(int j = 0; j < vv[i].size(); j++){
				for(int k = j+1; k < vv[i].size(); k++){
					if(j == k) continue;
					ans += is_anagram(vv[i][j].F, vv[i][j].S, vv[i][k].F, vv[i][k].S);
				}
			}
		}
		cout << ans << endl;
	}	
	return 0;
}
