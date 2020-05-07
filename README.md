skell: ДЗ 2 -- Используем язык для создания практичного приложения.

Второе домашнее задание проверяет понимание основных языка, что используются в
производственном программированиии. Обратите внимание, что задание соответствует
материалу, который рассказан в темах со 5 по 7
[отсюда](https://github.com/jagajaga/FP-Course-ITMO).

К заданию требуется создать тесты.

Документация в формате [Haddock](https://www.haskell.org/haddock/) будет
являться значимым бонусом.

Если вы видите неоднозначность в формулировке домашнего задания, то, пожалуйста,
обратитесь к преподавателям по электронной почте или в официальном канале в
slack.

Задача оценивается в максимум 10 баллов + бонусы.

# Срок сдачи

00:00 (UTC+3) 9 Мая 2020.

## Файловый менеджер и облегченная система контроля версий

Необходимо реализовать программу для манипуляции файлами на языке Haskell
используя конструкции монадных трансформеров и используя возможности IO монады.
Любые техники, что выходят за рамки текущего курса, будут приветствоваться.

Файловый менеджер будет атомарным и иметь простой функционал
просмотра/создания/удаления файлов и папок. Система контроля версий будет
следить за каждым файлом по отдельности и уметь их объединять.

Важно работать с файловой системой в чистом виде. Необходимо изначально считать
текущее состояние директории, а затем, после всех манипуляций записать новое
измененное состояние ~сознания~ файловой системы. Начните имплементацию с
формализации типа описания файловой системы и ее компонентов, а так же типов
для команд. После завершения программы, результаты ее действия должны
существовать в вашей файловой системе и директориях, которыми вы манипулировали.

Обратите внимание на обработку ошибок. Крайне нежелательно использование IORef и
подобных техник.

Информацию и состояние ревизий можно хранить рядом с рабочей директорией.

Для работы с реальной файловой системой используйте библиотеку
[directory](https://hackage.haskell.org/package/directory-1.3.6.1),
для парсинга аргументов командной строки мы рекомендуем
[optparse-applicative](https://hackage.haskell.org/package/optparse-applicative).

Необходимый функционал:

* command line interface (возможна реализация в виде интерактивной коммандной строки);
* переходить по директориям;
* показывать содержимое текущей директории;
* создать папку/файл;
* отобразить содержимое файла;
* удалить папку/файл;
* записать в файл текстовую информацию;
* поиск файла по названию  в текущей директории и ее подчастях и вывод пути до файла;
* отображать информацию о заданном файле:
    * путь;
    * права доступа;
    * тип файла;
    * время создания и/или изменения;
    * размер;    
* отображать информацию о директории:
    * размер;
    * путь;
    * количество файлов внутри;
    * права доступа;
* возможность инициализации системы контроля версий (СКВ) в текущей директории;
    * если текущая директория является частью уже инициализированной в СКВ, то
    инициализация не требуется;
* добавление файла или папки (всех файлов внутрии нее) в СКВ;
* добавления измененной версии файла и автоматической создание новой ревизии
данного файла;
    * добавление комментария к изменению;
* просмотр истории изменений файла;
    * подразумевается просмотр упорядоченного списка ревизий с комментариями;
* вывод конкретной версии файла по индексу в истории его изменений;
* объединение разных ревизий одного файла;
    * выбор приоритетного файла для объединения в случае конфликта;
    * бонусом будет иметь интерактивное объединение файлов, где пользователь
    будет по каждому случаю выбирать приоритет;
* удалить ревизию файла;
* удалить файл из СКВ;
* просмотр упорядоченную историю изменений файлов в заданной директории и ее
поддиректориях.

Дополнительные команды будут засчитаны как бонус к вашей реализации.

Весомым бонусом будет интерфейс для данной программы (TUI или GUI).

Пример:

```bash
$ my-best-file-manager ~/
/users/my_user/ > help
cd <folder> -- перейти в директорию
dir -- показать содержимое текущей директории
ls <folder> -- показать содержимое выбранной директории
create-folder "folder-name" -- создать директорию в текущей
cat <file> -- показать содержимое файла
create-file "file-name" -- создать пустой файл в текущей директории
remove <folder | file> -- удалить выборанную директорию или файл
write-file <file> "text" -- записать текст в файл
find-file "file-name" --  поиск файла в текущией директории и поддиректориях
information <file> -- показать информацию о файле
information <folder> -- показать информацию о директории
cvs-init -- инициализация СКВ в текущей выбранной директории
cvs-add <file | folder> -- добавление файла или папки в СКВ
cvs-update <file> "comment" -- добавление изменений файла в СКВ
cvs-history <file> -- просмотр истории изменений файла
cvs-cat <file> "index" -- просмотр конкретной ревизии файла
cvs-merge-revs <file> "index1" "index2" "left | right | both | interactive" --
объединение ревизий файла по заданным индексам, left, right, both или interactive
являются вариантами стратегий для обеъединения
cvs-delete-version <file> "index" -- удалить заданную версию файла из ревизий
cvs-remove <file> -- удалить файл из СКВ
cvs-show-everything -- показать общую историю изменений
help --  показать руководство по использованию
exit -- завершение работы программы
/users/my_user/ > cd folder-that-exist
/users/my_user/folder-that-exist > dir
a
b
c
/users/my_user/folder-that-exist > ls a
a is not a folder
/users/my_user/folder-that-exist > cat a
aaaaaa
/users/my_user/folder-that-exist > cd no-folder
no-folder does not exist
/users/my_user/folder-that-exist > create-file d
/users/my_user/folder-that-exist > dir
a
b
c
d
/users/my_user/folder-that-exist > cvs-init
/users/my_user/folder-that-exist > cvs-add a
/users/my_user/folder-that-exist > write-file a "a1a1a1"
/users/my_user/folder-that-exist > cvs-update a "new change!"
/users/my_user/folder-that-exist > cvs-history a
0. initial
1. new change!
/users/my_user/folder-that-exist > cvs-cat a "1"
a1a1a1
/users/my_user/folder-that-exist > cvs-cat a "0"
aaaaaa
/users/my_user/folder-that-exist > cvs-merge-revs a "0" "1" "both"
/users/my_user/folder-that-exist > cat a
aaaaaa
>>>>
a1a1a1
/users/my_user/folder-that-exist > exit
```
