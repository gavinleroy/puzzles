#include <iostream>
to
using namespace  std;

int main(void){
    
    float n;
    long long d;
    long long total = 1;
    
    cin >> n;
    
    while(total <= n)
        total *= 2;
    
    cout << total << endl;
    
    return 0;
}