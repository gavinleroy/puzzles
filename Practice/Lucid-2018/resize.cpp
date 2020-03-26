#include <iostream>
#include <string>
#include <cmath>

using namespace std;

void returnall(double setx, double sety, double &x2, double &y2, double ratio, double &w, double &h)
{
    if(abs(x2-setx)/abs(y2-sety) >= ratio)
    {
        w = abs(x2 - setx);
        h = (double)(abs(x2 - setx)/ratio);
    }
    else
    {
        w = abs(y2-sety)*ratio;
        h = abs(y2-sety);
    }
}

int main(void)
{
    double x, y, w, h, x2, y2, t, setx, sety, finx, finy;
    double ratio;
    string s;
    cin >> t;
    for(int i = 0; i < t; ++i)
    {
        cin >> x >> y >> w >> h;
        cin >> s;
        cin >> x2 >> y2;
        ratio = w/h;
        if(s == "BottomRight")
        {
            setx = x;
            sety = y;
            returnall(setx, sety, x2, y2, ratio, w, h);
        }
        else if(s == "BottomLeft")
        {
            setx = x+w;
            sety = y;
            returnall(setx, sety, x2, y2, ratio, w, h);
        }
        else if(s == "TopLeft")
        {
            setx = x+w;
            sety = y+h;
            returnall(setx, sety, x2, y2, ratio, w, h);
        }
        else if(s == "TopRight")
        {
            setx = x;
            sety = y+h;
            returnall(setx, sety, x2, y2, ratio, w, h);
        }
        x=setx;
        y=sety;
        if(x > x2)x = x2;
        if(y > y2)y = y2;
        cout << (int)x << " " << (int)y << " " << (int)w << " " << (int)h << endl;
    }
    
    return 0;
}