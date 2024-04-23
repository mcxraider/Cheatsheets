
Qn1s to take note:
apr18 - 1B
apr18 - 1c




a = [1, 2, [3, 4]]
b = a.copy()
a[0] = 5 a[2][0] = 6 
print(a) 
print(b)
>> [5, 2, [6, 4]]
>> [1, 2, [6, 4]]
The inner bracket of [6,4] is what doesnt changes too. if something in [6,4] changes, it changes in b too

---

a = [1,2,3,4]
print(a[:5])
This does not throw an index error. Slicing above the limit will just return the full list 
>> [1,2,3,4]

---

## Types of errors:

- TypeError: Raised when an operation or function is applied to an object of an inappropriate type.
- ValueError: Raised when a function receives an argument of the correct type but an inappropriate value.
- NameError: Raised when a local or global name is not found. This can occur when a variable is used before it has been defined.
- IndexError: Raised when a sequence subscript is out of range.
- KeyError: Raised when a dictionary key is not found.
- ZeroDivisionError: Raised when the second argument of a division or modulo operation is zero.
- RecursionError: Raised when the interpreter detects that the maximum recursion depth is exceeded.
- AttributeError: Raised when an attribute reference or assignment fails.


---

print([4] * 0)
multiplying list by 0 returns empty list
>> []


---

When evaluating string of lambdas evaluate left to right 

---

Starting to index from outside a list just returns an empty list 
a = [1,2,3]
print(a[3:])
>> []

---

Transposing a matrix, works for nxn and nxm
```
def transpose(matrix):
   transposed_matrix = []
   for i in range(0,len(matrix[0]),1):
       new_row = []
       for j in range(0,len(matrix),1):
          new_row.append(matrix[j][i])
       transposed_matrix.append(new_row)
   return transposed_matrix

```

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

Doing 3 different sorts on same iterable:
function returns a tuple of pairs (level,count), where the entries are sorted first in descending order
by count, then in ascending order for the achievement level for each count. 
Sample result:
>>> ((4, 3), (1, 2), (7, 2), (8, 2), (5, 1), (6, 1))

def score_distribution(table):
   results = list(table.items())
   results.sort()
   results.sort(reverse=True, key=lambda x:x[1])
   return tuple(results)

---






































