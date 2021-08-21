// runs in browser
((C,O={})=>{let P=[],F=[],G=[],W=[],V=[],I=0,E="(s.pop()??0)",J=e=>
`s.push(${e})`,B=e=>`A=${E},B=${E},`+J(e),U=e=>`A=${E},`+J(e),T=i=>
`p=${i};break`,A=c=>_=>T(C.indexOf(c,I)+1||-1),K=(p,i)=>`if(${p
}s[~-s.length]){${T(i)}}`,D=(p,a,b)=>i=>(P[i=a.pop()]=P[i].replace
(-1,I),K(p,b.pop())),H=(p,a,b)=>_=>(a.push(I),b.push(P.length+1)
,K(p,-1)),L={'%':B`A%B`,"'":A`:`,',':J`+i('input number')`,'.':
"o"+E,':':D("!",W,V),';':D("0!=",F,G),'^':A`^`,a:B`A+B`,b:T(E),c:
U`A),s.push(A`,d:B`A/B`,e:B`A**B`,f:H("0==",G,F),g:B`+(A>B)`,h:T(-1)
,i:J`A=i('input char')?A.charCodeAt(0):-1`,j:J`s.length`,k:A`;`,l:B`
+(A==B)`,m:B`A*B`,n:U`+!A`,o:`o(String.fromCharCode${E})`,p:U`A+1`,q
:"for(let c of i('input chars'))"+J`c.charCodeAt(0)`,r:E,s:B`A-B`,t:
J`1+Math.floor(10*Math.random())`,u:U`A-1`,v:U`Math.sqrt(A)`,w:H("",
V,W),x:U`s.splice(A,1)[0]`,y:B`...s.splice(A,0,B)`,z:B`A,B`};P.push(
`(i,o)=>{let p=0,s=[],t,A,B;for(;!t;${O.d?"o(s)":""})switch(p){\n`);
for(let c of C)P.push(`case ${I++}:`,/\d/.test(c)?`s.push(${c});\n`:
c in L?(c=L[c],c.call?c():c)+";\n":"");P.push`default:t=1}\nconsole.log(s)}`
;P=P.join("");return O.c?P:eval(P)(O.i??prompt,O.o??console.log)})
