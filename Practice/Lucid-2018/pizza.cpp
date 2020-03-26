#include <iostream>
#include <cmath>

using namespace std;

int main(void){
    int t;
    
    cin >> t;
    
    while(t--){
        long long p, m, n, temp;
        cin >> p >> m >> n;
        bool turn = true;
        
        if((p/n) % 2 == 1) turn = !turn;
        
        p %= n;
        
        p += n;
        
        int i = m;
        while(p - i - m >= 0 && i <= n){
            i++;
        }
        p -=i;
        turn = !turn;

        if(p > 0){
            i = m;
            while(p - i - m >= 0 && i <= n){
                i++;
            }
            turn = !turn;
        }
        
//        temp = p-n;
        
//        if((n - abs(temp)) >= m){
        if(p >= 0){
            if(turn)
                cout << "Megan\n";
            else
                cout << "Jacob\n";
        }else{
            if(!turn)
                cout << "Megan\n";
            else
                cout << "Jacob\n";
        }
    }
    
    return 0;
}