#include <map>
#include <vector>
#include <string>

class Aaagmnrs{
	std::string[] anagrams(std::string ss[]){
		std::vector<std::string> res;
		map<int, std::vector<std::vector<int> > > mm;
		for(element : ss){
			std::vector<int> alph(26, 0);
			int sum = 0;
			for(int i = 0; i < element.size(); i++){
				sum += toupper(element[i]);
				alph[toupper(element[i])-'A']++;	
			}
			if(mm.count(sum)){
				bool broke = false;
				for(int i = 0; i < mm[sum].size(); i++){
					for(int k = 0; k < mm[sum][i].size(); k++){
						if(mm[sum][i][k] != alph[k]){
							broke = true;
							break;
						}	
					}
					if(broke){
						break;
					}
				}	
				if(!broke) res.push_back(element);
			}else res.push_back(element);
			mm[sum].push_back(alph);
		}	
		std::string rs[res.size()];
		for(int i = 0; i < res.size(); i++) rs[i] = res[i];
		return rs;
	}
};
