# MarkovAlgorithms
Программа, считывающая схему нормального алгорифма Маркова из файла и позволяющая применять этот алгорифм к строке, введённой пользователем.
 
# Вид файла с алгорифмом:
1 строка - алфавит.
2 строка - расширение алфавита.
Последующие строки: <слово> -> <слово> ИЛИ <слово> ->. <слово>
Пустое слово - пустая строка.

# Аргументы командной строки:
<имя текстового файла со схемой алгорифма> <режим работы программы>

# Режимы работы:
Всего есть 2 режима: res и comp.
В режиме res программа сразу выдаёт результат применения алгорифма к строке.
В режиме comp программа показывает последовательность промежуточных вычислений и сам результат(он находится в конце списка)
применения алгорифма.
Работа программы построена следующим образом:
Она ожидает ввод строки. Если введённая строка - "exit" в любом регистре, то осуществляется выход из программы.
Если введена иная строка, то программа проверяет, что эта строка является словом алфавита и в противном случает выдаёт 
соответствующее сообщение. Если введённая строка является словом алфавита, то программа выдаёт результат применения алгорифма,
считанного из файла, к данному слову в виде, соответствующем выбранному режиму. Затем программа опять ожидает ввод строки. 
И так далее, пока программа не получит указание к остановке, описанное выше.
