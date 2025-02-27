**Статус проекта : завершен.** <br />
Перед вами исходный код проведенного анализа временных рядов динамики численности
мелких млекопитающих центральной Сибири. Результаты являются основой одной из глав
[диссертации на соискание ученой степени кандидата биологических наук](<https://sev-in.ru/sites/default/files/2024-12/%D0%AF%D0%BA%D1%83%D1%88%D0%BE%D0%B2_%D0%B4%D0%B8%D1%81%D1%81%D0%B5%D1%80%D1%82%D0%B0%D1%86%D0%B8%D1%8F_6.12.pdf>),
а также [опубликованы](https://onlinelibrary.wiley.com/doi/10.1111/1749-4877.12770) в высокорейтинговом международном журнале Integrative Zoology. <br />

Ссылка для цитирования: Yakushov, Vasily D., and Boris I. Sheftel. "Are population cycles recovering?." Integrative Zoology 19.3 (2024): 538-547. <br />

В репозитории содержится R-проект, созданный в RStudio. Для полной воспроизводимости кода рекомендуется запускать файл `are_population_cycles_recovering.Rproj`, а уже затем скрипт из папки `scripts`. <br />

## Структура репозитория

### `scripts`

Содержит единственный R-файл `wavelet coherency + dominance structure.r`. Проведен анализ вейвлет-когерентности временных рядов динамики численности мелких млекопитающих на разных берегах Енисея. Полученные спектрограммы позволяют судить, были ли колебания периодичными и синхронными на разных берегах, а также определить величину периода и его временную локализацию. Детальное описание метода применимо к анализу экологических временных рядов приведено [здесь](https://www.researchgate.net/publication/5529059_Wavelet_analysis_of_ecological_time_series). Интерпретацию результатов см. в тексте кандидатской диссертации и опубликованной статьи (ссылки см. выше).

---
### `initial data`
Каталог, содержащий исходные данные о численности млекопитающих. <br />
**Внимание, в данные добавлен шум! Таким образом, что общая картина временных рядов сохранена,
но абсолютные значения численности отличаются от реальных!** <br />

Префикс "LB" в названии файла означает, что данные получены с левого берега Енисея, а "RB" - с правого.
"XX" или "XXI" в названии файла обозначает временной промежуток, к которому относятся ряды.

---

### `Images`

Содержит графики, полученные в результате проведенного анализа.
Включает в себя подкаталоги: 
#### `numbers` 
Содержит файлы: <br /> 
`proportion_graph.png` - структура доминирования (доля вида в сообществе) по берегам. 
![](https://raw.githubusercontent.com/yakushov1/time_series_of_dynamics/refs/heads/main/images/numbers/proportion_graph.png)
<br /> 
`rodent_average_wavelet_and_numbers.png` - численность грызунов на разных берегах (секция B)
и осредненные кросс-вейвлеты для XX (секция A) и XXI века (секция C). <br /> 
`shrew_average_wavelet_and_numbers.png` - то же, но для землероек. 
![Пример графика](https://raw.githubusercontent.com/yakushov1/time_series_of_dynamics/refs/heads/main/images/numbers/rodent_average_wavelet_and_numbers.png)
#### `spectrograms` 
Разбит на подкаталоги, соответствующие временным промежуткам, 
к которым относятся проанализированные временные ряды. 
Каждый файл именован по видовому названию и представляет собой кросс-вейвлет спектрограмму,
отражающую периодичность и синхронность колебаний численности одного и того же вида на разных 
берегах Енисея.


