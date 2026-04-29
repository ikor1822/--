# 💻 Примеры программ на Gopi

Эти примеры демонстрируют полноту языка по Тьюрингу и работу со сложными структурами данных.

## 1. Вычисление факториала (Рекурсия)
Классический пример функционального программирования.
```javascript
func fact(n) {
    if n == 0 { 
        1 
    } else { 
        n * fact(n - 1) 
    }
}

print("Факториал 6:")
print(fact(6)) // 720
```

## 2. Алгоритм быстрой сортировки (Quicksort)
Демонстрирует работу функций `filter`, `head`, `tail` и конкатенацию списков. Обратите внимание на использование анонимных лямбда-функций прямо в аргументах вызова.
```javascript
func qs(arr) {
    if len(arr) == 0 {
        []
    } else {
        let pivot = head(arr)
        let rest = tail(arr)
        
        let less = filter(fun(x) { x < pivot }, rest)
        let eq = filter(fun(x) { x == pivot }, rest)
        let greater = filter(fun(x) { x > pivot }, rest)
        
        qs(less) + [pivot] + eq + qs(greater)
    }
}

let numbers = [8, 3, 10, 1, 9, 3, 2, 7]
print("Отсортированный массив:")
print(qs(numbers)) 
// Вывод: [1, 2, 3, 3, 7, 8, 9, 10]
```

## 3. Использование Map и Fold
```javascript
let numbers = [1, 2, 3, 4, 5]

// Возведение в квадрат (map)
let squares = map(fun(x) { x * x }, numbers)
print(squares) // [1, 4, 9, 16, 25]

// Суммирование элементов (fold)
let sum = fold(fun(acc, x) { acc + x }, 0, numbers)
print(sum) // 15
```