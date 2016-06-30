def rem(t,a):
    return t[a:len(t)]

while True:

    p = input("\n")
    t = p[0:len(p)] # I want to be able to save it for later

    while len(t) > 0:
    
        if t[0] == "/":
        
            t = rem(t,1)
        
            pat = ""
            while not t[0] == "/":
                if t[0] == "\\": # escape characters
                    pat += t[1]
                    t = rem(t,2)
                else:
                    pat += t[0]
                    t = rem(t,1)
        
            t = rem(t,1)

            rep = ""
            while not t[0] == "/":
                if t[0] == "\\": # escape characters
                    rep += t[1]
                    t = rem(t,2)
                else:
                    rep += t[0]
                    t = rem(t,1)

            t = rem(t,1)

            while pat in t:
                t = t.replace(pat,rep,1)
                
        elif t[0] == "\\": # escape characters
            print(t[1])
            t = rem(t,2)
        
        else:
            print(t[0])
            t = rem(t,1)
