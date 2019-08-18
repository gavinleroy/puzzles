#/usr/bin/python3

class Aaagmnrs:
    def __init__(self):
        self.results = []
    def anagrams(self, ss):
        res = []
        mm = {}
        for element in ss:
            alph = {}
            sum = 0
            for char in element:
                sum += ord(char.upper())
                if ord(char.upper())-ord('A') not in alph:
                    alph[ord(char.upper())-ord('A')] = 1
                else:
                    alph[ord(char.upper())-ord('a')] = alph[ord(char.upper())-ord('A')] + 1

            if sum in mm:
                broke = False
                for alph_ in mm[sum]:
                    for char, qty in alph_.items():
                        if alph[char] is not qty:
                            broke = True
                            break
                    if broke:
                       break
                if not broke:
                    res.append(alph)
                mm[sum].append(alph)
            else:
                temp = []
                temp.append(alph)
                mm[sum] = temp
                res.append(element)
        self.results.append(res)
        return res 

