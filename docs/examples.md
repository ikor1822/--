# Примеры программ

Данные алгоритмы демонстрируют вычислительную полноту языка и возможности работы с рекурсией и функциями высшего порядка.

## 1. Вычисление факториала
Пример использования условных операторов и рекурсивных именованных функций (`func`).

```javascript
func fact(n) {
    if n == 0 { 
        1 
    } else { 
        n * fact(n - 1) 
    }
}

print("Факториал числа 6:")
print(fact(6)) // Ожидаемый результат: 720
```

## 2. Быстрая сортировка (Quicksort)
Алгоритм демонстрирует мощь стандартной библиотеки. Используется извлечение головы и хвоста списка, конкатенация массивов и фильтрация с использованием анонимных функций (`fun`).

```javascript
func qs(arr) {
    if len(arr) == 0 {
        []
    } else {
        pivot = head(arr)
        rest = tail(arr)
        
        less = filter(fun(x) { x < pivot }, rest)
        eq = filter(fun(x) { x == pivot }, rest)
        greater = filter(fun(x) { x > pivot }, rest)
        
        qs(less) + [pivot] + eq + qs(greater)
    }
}

numbers = [8, 3, 10, 1, 9, 3, 2, 7]
print("Отсортированный массив:")
print(qs(numbers)) 
// Ожидаемый результат: [1, 2, 3, 3, 7, 8, 9, 10]
```

## 3. Обработка данных (Map / Fold)
```javascript
numbers = [1, 2, 3, 4, 5]

squares = map(fun(x) { x * x }, numbers)
print("Квадраты чисел:")
print(squares) // [1, 4, 9, 16, 25]

sum = fold(fun(acc, x) { acc + x }, 0, numbers)
print("Сумма элементов:")
print(sum) // 15
```