
Qn1s to take note:
apr18 - 1B
apr18 - 1c
nov21 - 1B
Nov21 - 1E

---

a = [1,2,3,4]
print(a[:5])
This does not throw an index error. Slicing above the limit will just return the full list 
>> [1,2,3,4]

---

print([4] * 0)
multiplying list by 0 returns empty list
>> []

Starting to index from outside a list just returns an empty list 
a = [1,2,3]
print(a[3:])
>> []

---

Sometimes its easier to convert the datatype to something else so that u can manipulate it easily. But remember to convert it back.

---

Dictionary cannot be modified during iteration.
Will get RuntimeError
This code causes this error:
d = {1:2, 3:4, 5:6} 
for k, v in d.items():
   d[v//2] = k 
print(d)
Moral of story: you shouldn't add or remove keys during iteration. dictionary cannot change size during iteration

---

Qn 1: Watch out for None returns if there is no return clause for the function

---

It is possible to have 2 returns to error calls

def bar(x):
    try:
        return x * 2
    except TypeError:
        return 'type'
    except ValueError:
        return 'value'
    except Exception:
        return 'except


print(bar(bar(bar)))
>>"typetype"


---

If something is continually being printed forever, write 
"inifinite {That something being printed}"


lst1 = [1, 2, 3, 4]
lst2 = [5, 6, 7, 8]
for i in lst1:
   lst2.append(i)
   lst1.remove(i) 
print(lst1)
print(lst2)

Ans is [2,4] and [5,6,7,8,1,3]

Thus, the element 2 is skipped because after the removal of 1 from lst1, the list structure changes and the iteration index moves forward to the next index, which now points to the number 3 instead of 2.

---

def sherlock(*args*):
    try:
        print("Deduction: " + args[1] + args[-1])
    except ZeroDivisionError:
        print("It is")
    except TypeError: 
        print("elementary")
    except Exception:
        print("my dear")
    except IndexError: 
        print("Watson")
    finally:
        return (args[0],)

print(sherlock("holmes"))
If the except Exception comes before the actual error raised, then that clause will run
>>> my dear
>>>("holmes",)

---

def test(x): 
   try:
      return x[0] / x[1] 
   except ZeroDivisionError:
      print("Zero!") 
   except IndexError:
      print("Index!") 
   finally:
      return x[1] / x[0] 
print(test((1, 0)))

>> Zero!
>> 0.0

If only 1 divider, it will be 0.0

---
Revise:
- Apr 19 dict qn
- Apr 20 list qn



Write in:
- diff ways to create a dictionary






