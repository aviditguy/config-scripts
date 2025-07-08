#set page(width: 750pt, height: 5000pt)

= Number System and Conversion

#outline()

== 1. Introduction
+ *Decimal Number* #h(40pt) Base $= 2$, means it has $2$ digits: $0, 1$
+ *Binary Number* #h(47pt) Base $=10$, means it has $10$ digits: $0, 1, 2, 3, 4, 5, 6, 7, 8, 9$
+ *Hexadecimal Number* #h(16pt) Base $=16$, means it has $16$ digits: $0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "A, B, C, D, E, F"$

- Binary Numbers is what used by Computers to represent data, not easy to read and write.
- Decimal Numbers is what we are comfortable with but computers do not represent data in Decimal
- Hexadecimal Numbers is the middle ground we can easily convert to and from Binary, easier to read and write then Binary

==== Relation Between Decimal Hexadecimal and Binary

#table(
  columns: 17,
  [*Decimal*],[$0$],[$1$],[$2$],[$3$],[$4$],[$5$],[$6$],[$7$],[$8$],[$9$],[$10$],[$11$], [$12$],[$13$],[$14$],[$15$],
  [*Hex*],[$0$],[$1$],[$2$],[$3$],[$4$],[$5$],[$6$],[$7$],[$8$],[$9$],[$"A"$],[$"B"$], [$"C"$],[$"D"$],[$"E"$],[$"F"$],
  [*Decimal*],[$0000$],[$0001$],[$0010$],[$0011$],[$0100$],[$0101$],[$0110$],[$0111$],[$1000$],[$1001$],[$1010$],[$1011$], [$1100$],[$1101$],[$1110$],[$1111$],
)

#v(1em)
#line(length: 100%)
#v(1em)

== 2. Convert Decimal $100.36$ to Binary
+ Split Decimal into Integer $(100)$ and Fractional $(0.36)$ parts
+ Repeatedly Divide $100$ by $2$ until its $0$ and read the remainder from bottom to top
+ Multiply $0.36$ by $2$ and extract the Integer Part from the Product
+ Repeat it with Fractional Part of Product until its $0$ or upto few steps and read extracted integer part from top to bottom
*Binary Representation of $100.36 = (1100100.01011100)_(2)$*
#image("Assets/DecimalToBinary.png")

== 3. Convert Decimal $100.36$ to Hexadecimal
+ Same as *Decimal To Binary* just use Hexadecimal's Base $= 16$
*Hexadecimal Representation of $100.36 = (64.5"C"28"F"5"C"2)_(16)$*
#image("Assets/DecimalToHexadecimal.png")

#v(1em)
#line(length: 100%)
#v(1em)

== 4. Convert Binary $(1100100.01011100)_(2)$ to Decimal
+ Split Binary into Integer and Fractional Parts
+ Indexing in Integer Part starts at $0 "to" n$ from right to left and at $-1$ from left to right in Fractional Part
+ Add all the bits, value of each bit is $= "bit" dot 2^("index")$
*Decimal Representation of $1100100.01011100 = (100.36)_(10)$*
#image("Assets/BinaryToDecimal.png")

== 5. Convert Binary $(1100100.01011100)_(2)$ to Hexadecimal
+ Each hex corresponds to $4$ binary digits.
+ For Integer Part, group from right to left and left to right for Fractional Part
+ If there is no sufficient bits pad them with $0$s
*Hexadecimal Representation of $1100100.01011100 = (64.5"C")_(16)$*
#image("Assets/BinaryToHexadecimal.png")

#v(1em)
#line(length: 100%)
#v(1em)

== 6. Convert Hexadecimal $(64.5"C")_(16)$ to Binary
+ Expand each hex digit to its corresponding $4$ bit binary
*Binary Representation of $64.5"C" = (1100100.01011100)_(2)$*
#image("Assets/HexadecimalToBinary.png")

== 7. Convert Hexadecimal $(64.5"C")_(16)$ to Decimal
+ There is no direct way to convert Hex into Decimal
+ First convert it to Binary and then from Binary to Decimal
*Decimal Representation of $64.5"C" #sym.arrow.r (1100100.01011100)_(2) #sym.arrow.r (100.36)_(10)$*

#v(1em)
#line(length: 100%)
#v(1em)

== 8. Convert Decimal when it is of the form $x = 2^n$
When decimal is of the form $x=2^n$ we can directly convert it to Binary and Hexadecimal
+ *Conversion To Binary is simple $1$ followed by $n$ zeros.*
  - $256 #sym.arrow.r 2^8 #sym.arrow.r (100000000)_(2)$ #h(40pt) $1$ followed by $8$ zeros
  - $1024 #sym.arrow.r 2^10 #sym.arrow.r (10000000000)_(2)$ #h(20pt) $1$ followed by $10$ zeros
+ *Conversion To Hexadecimal*
  - $2^7 #sym.arrow.r underbrace(1 #h(4pt) 0 #h(4pt) 0 #h(4pt) 0, "8") #h(4pt) underbrace(0 #h(4pt) 0 #h(4pt) 0 #h(4pt) 0, "0") #sym.arrow.r (80)_(16)$ 
  - $x = 2^n$ we can write $n=i+4j$
  - $j$ represents number of zeros, $j=1 #sym.arrow.r 0$ and $j=2 #sym.arrow.r 00$ and so on
  - $3 <= i <= 0$ represents the leading $1$ part:
    - $i=0 #sym.arrow.r (1)_(16) #sym.arrow.r 1$
    - $i=1 #sym.arrow.r (10)_(16) #sym.arrow.r 2$
    - $i=2 #sym.arrow.r (100)_(16) #sym.arrow.r 4$
    - $i=3 #sym.arrow.r (1000)_(16) #sym.arrow.r 8$

  - $2048 #sym.arrow.r 2^11 #sym.arrow.r (100000000000)_(2)$
    - $11 = 4 dot 2 + 3 #h(20pt) j=2, i=3$
    - *Hexadecimal Representation* $(800)_(16)$

#v(1em)
#line(length: 100%)
#v(1em)

== 9. Binary Arithmetic
+ *Binary Addition*
  - $0+0=0$
  - $1+0=1$
  - $1+1=2$ which is $10$ in binary which is $0$ with a carry of $1$
  - $1+1+1=3$ which is $11$ in binary which is $1$ with a carry of $1$
  #image("Assets/BinaryAddition.png")

+ *Binary Subtraction*
  - $0-0=0$
  - $1-0=1$
  - $1-1=0$
  - $0-1$ we can't do so we borrow $1$ from next column. This makes it $10-1=1$
  #image("Assets/BinarySubtraction.png")

+ *Binary Multiplication*
  - $0 times 0=0$
  - $1 times 0=0$
  - $1 times 1=1$
  #image("Assets/BinaryMultiplication.png")

+ *Binary Division*
  #image("Assets/BinaryDivision.png")

#v(1em)
#line(length: 100%)
#v(1em)

== 10. Hexadecimal Arithmetic
+ *Hexadecimal Addition*
  - $1+1=2$
  - $9+1="A"$
  - $"F"+1=10$ which is $15+1=16$ in decimal convert it to hex $16-16=0$ this is $0$ with carry $1$ which is $10$
  - $"A"+"B"=15$ which is $10+11=21$ in decimal convert it to hex $21-16=5$ this is $5$ with carry $1$ which is $15$
  - $"F"+"F"=1"E"$ which is $15+15=30$ in decimal convert it to hex $30-16=14$ this is $14("E")$ with carry $1$ which is $1"E"$
  #image("Assets/HexadecimalAddition.png")

+ *Hexadecimal Subtraction*
  - $1-1=0$
  - $"A"-1=9$ which is $10-1=9$ in decimal and also in hex
  - $1-"F"=2$
    - It is $1-15$ in decimal we take carry $1$ from next column
    - In decimal taking carry $1$ is $10+1=11$ which will be $11-15$
    - In hexadecimal taking carry $1$ is $16+1=17$ which will be $17-15=2$
  - $"A"-"B"=-1$ if no carry with carry it will be $"A"-"B"="F"$ which will be $16+10-11=15$ which is $"F"$
  #image("Assets/HexadecimalSubtraction.png")

+ *Hexadecimal Multiplication and Division*
  - Just convert to Decimal perform calculation and convert to Hexadecimal

#v(1em)
#line(length: 100%)
#v(1em)

== 11. C Program to convert Decimal To Binary
// #let code = read("code.c")
// #raw(code,lang:"c")

