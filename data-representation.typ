#set page(width: 750pt, height: 5000pt)

#heading(outlined: false)[Data Representation in Computer and C]

#outline()

= 1. Unsigned Data Representation
+ Unsigned Numbers are positive numbers
+ Min and Max value of $n$ bit Unsigned Integer is $[0 "to" 2^n-1]$

== 1.1. C Program to print Binary Representation of Unsigned Integers

#v(1em)
#line(length: 100%)
#v(1em)

= 2. Signed Data Representation
+ Signed Numbers handle both +ve and -ve numbers, there range is smaller than unsigned Numbers
+ The Left most bit is called *MSB (Most Significant Bit)* as this bit holds the highest value
+ Right most bit is called *LSB (Least Significant Bit)*
+ In $n$ bit Signed Number MSB is reserved as *sign bit* $1$ for -ve and $0$ for +ve numbers 
+ Rest $n-1$ bit hold the value of number
+ Range of $n$ bit Signed Number is $[]$

#v(1em)
#line(length: 100%)
#v(1em)

= 3. Floating Point Number Representation
