#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <iomanip>
#include <memory>

/*******************************************************
*           ADVENT OF CODE DAY 19 C++
*
*
*
*
*
*
*
*
*******************************************************/

bool findDuplicate(std::vector<int*> addresses, int data[5]){
    std::cout << "inside findDuplicate" << std::endl;
    bool isThere = false;
    bool eachOne = false;
    
    for(unsigned int i = 0; i < addresses.size(); ++i){
        for(unsigned int k = 1; k < 5; ++k){
            std::cout << "addresses[i][k]: " << addresses[i][k] << " data[k]: " << data[k] << std::endl;
            if(addresses[i][k] != data[k]){
                eachOne = true;
            }
        }
        if(eachOne){
            break;
        }else{
            std::cout << "The one above suposably worked" << std::endl;
            isThere = true;
        }
    }
    std::cout << std::endl;
    std::cout << "returning: " << isThere << std::endl;
    return isThere;
}




void calculateDistance(const int data[5]){
    int northSouth = 0,
    eastWest = 0;
    
    (data[1] - data[3] < 0) ? northSouth = ((data[1] - data[3]) * -1) : northSouth = (data[1] - data[3]);
    (data[2] - data[4] < 0) ? eastWest = ((data[2] - data[4]) * -1) : northSouth = (data[2] - data[4]);
    
    std::cout << "Distance is: " << northSouth + eastWest << std::endl;
}

void pushCoord(int data[5], std::vector<int>& n, std::vector<int>& s, std::vector<int>& e, std::vector<int>& w){
    
}

void readFile(std::vector<int*>& address, int (&data)[5]){
    
    std::string myString;
    int tempArray [5] = {0, 0, 0, 0, 0};
    std::vector<int*> newVector;
    newVector.push_back(tempArray);
    
    std::ifstream getWords("/users/gavingray/Desktop/input.txt");
    
    std::string tempString;
    int tempInt = 0;
    
    while(getWords >> myString){
        tempString = "";
        
        switch(myString[0]){
            case 'L':
                if(data[0] == 0)
                    data[0] = 3;
                else
                    data[0] -= 1;
                break;
            case 'R':
                data[0] = (data[0] + 1) % 4;
                break;
            default:
                std::cout << "Something horrible happened" << std::endl;
                break;
        }
        //get rid of the first R, L
        for(unsigned int i = 1; i < myString.length(); ++i){
            tempString += myString[i];
        }
        //get the integers out of the string
        std::istringstream ss(tempString);
        ss >> tempInt;
        //update the coordinates
        data[data[0] + 1] += tempInt;
        
        
        for(unsigned int g = 0; g < 5; ++g){
            tempArray[g] = data[g];
        }
        newVector.push_back(tempArray);
        
        std::cout << "calling the function" << std::endl;
        if(findDuplicate(newVector, tempArray)){
            std::cout << "Here is your answer: ";
            calculateDistance(data);
            break;
        }else{
            std::cout << "pushing back data: " << std::endl;
            address.push_back(tempArray);
        }
        
        ss.clear();
    }
    
    getWords.close();
}


int main(void){
    
    int data [5] = {0, 0, 0, 0, 0};
    
    std::vector<int> north;
    std::vector<int> south;
    std::vector<int> east;
    std::vector<int> west;
    
    pushCoord(data, north, south, east, west);
    
    
    readFile(addresses, data);
    
    //used for the first part of day 1
    //calculateDistance(data);
    
    return 0;
}