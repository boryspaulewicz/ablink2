##! TODO Liczba prób, sprawdzone słowa, sprawdzić, czy wszystkie słowa
##się pojawiają, czy nie ma NA.

## Procedura: attentional blink a potem pamięciowe, w pamięciowym po
## 10 słów w każdej kategori (neg, neu, poz)

## Koster: T1 był poz, neu lub neg, po 9 samoopisowych przymiotników,
## T2 zawsze neu.

## Słowa T1 miały długość 4-6 znaków, wybrane w oparciu o oceny emocji
## i familiarności.

## T2 był wybierany z zestawu 27 słów neutralnych 3-6 znaków. Każde ze
## słów T1 i T2 powtórzyło sie 5 razy. Jako dystraktory użyto zestaw
## 79 długich słów mało znanych, 9-18 znaków.

## Liczba prób: 10 prób treningowych i 135 właściwych (135 / 27 = 5).

library(stringr)
## library(openxlsx)
if(interactive())source('~/cs/code/r/tasks/task/task.R')
## db.connect('task')
TASK.NAME <<- 'ablink'

FIXATION.TIME = 1000
POST.FIXATION.TIME = 1000
PRESENTATION.TIME = 96
ISI = 16
NOF.ITEMS = 13

## Wczytujemy słowa z bazy i przygotowujemy zestaw bodźców
ct = function(x)x - mean(x, na.rm = TRUE)
st = function(x)ct(x) / sd(x, na.rm = TRUE)
words = readRDS('nawl.rds')
words = words[words$Gram == 3,]
words$val = st(words$val_M_all)

## Tylko według kryterium .5 wychodzi dokładnie 27 słów neutralnych - tyle ich potrzeba dla T2
neu = words$NAWL_word[abs(words$val) < .5]
neu.len = str_length(neu)
neu = neu[neu.len <= 6]

## Dystraktory
dis = words$NAWL_word[abs(words$val) < .5]
dis.len = str_length(dis)
dis = (dis[dis.len >= 9])[1:79]

words = read.csv('short_words.csv', header = T)
names(words)[3] = 'pos'

## df = data.frame(item = 1:max(length(neg), length(neu), length(pos), length(dis)))
## df$neg = df$neu = df$pos = df$dis = ""
## df$neg[1:length(neg)] = neg
## df$neu[1:length(neu)] = neu
## df$pos[1:length(pos)] = pos
## df$dis[1:length(dis)] = dis
## write.xlsx(df, file = 'nawl_words.xlsx')

WINDOW$set.visible(T)
WINDOW$set.mouse.cursor.visible(T)

FX = fixation(WINDOW, size = .02)

WORDS.I = list()
INDICES = list()
trial.code = function(trial, t1em = sample(c('neg', 'neu', 'pos'), 1), t1pos = sample(3:5, 1), t2lag = sample(c(2, 4, 6), 1), i = 1){
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    t2pos = t1pos + t2lag
    t1em = as.character(t1em)
    if(trial == 1){
        ## Słowa losowane z minimalizacją powtórzeń
        WORDS.I <<- list(neg = scen(9, 1, 5), neu = scen(9, 1, 5), pos = scen(9, 1, 5), t2 = scen(27, 1, 5), dis = scen(79, 1, 100))
        INDICES <<- list(neg = 1, neu = 1, pos = 1, t2 = 1, dis = 1)
        state = 'press-space'
    }else{ state = 'show-fixation' }
    if(WINDOW$is.open())process.inputs()
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        ## Możliwość wyjścia z etapu za pomocą ESC
        if(KEY.PRESSED[Key.Escape + 1] > start)return(NULL)
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            TXT$set.string("Naciśnij spację")
            center.win(TXT)
            WINDOW$clear(c(0, 0, 0))
            WINDOW$draw(TXT)
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(0, 0, 0))
            lapply(FX, WINDOW$draw)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                state = 'post-fixation'
                fixation.cleared = CLOCK$time
            }
        }, 'post-fixation' = {
            if((CLOCK$time - fixation.cleared) > POST.FIXATION.TIME){
                state = 'show-stim'
                item = 1
            }
        }, 'show-stim' = {
            WINDOW$clear(c(0, 0, 0))
            ## Targety rysujemy na zielono
            if(item %in% c(t1pos, t2pos)){
                TXT$set.color(c(0, 1, 0))
            }else{
                TXT$set.color(c(1, 1, 1))
            }
            ## Pierwszy target ma kontrolowaną walencję
            if(item == t1pos){
                t1stim = stim = as.character(words[[t1em]][WORDS.I[[t1em]][INDICES[[t1em]]]])
                INDICES[[t1em]] <<- INDICES[[t1em]] + 1
            }else if(item == t2pos){
                t2stim = stim = neu[WORDS.I$t2[INDICES$t2]]
                INDICES$t2 <<- INDICES$t2 + 1
            }else{
                stim = dis[WORDS.I$dis[INDICES$dis]]
                INDICES$dis <<- INDICES$dis + 1
            }
            TXT$set.string(stim)
            bounds = TXT$get.local.bounds()
            TXT$set.origin(c(bounds['width'] / 2, bounds['top'] + bounds['height'] / 2))
            TXT$set.position(WINDOW$get.size() / 2)
            WINDOW$draw(TXT)
            WINDOW$display()
            stim.onset = CLOCK$time
            state = 'stim-present'
        }, 'stim-present' = {
            if((CLOCK$time - stim.onset) >= PRESENTATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                stim.cleared = CLOCK$time
                state = 'stim-cleared'
            }
        }, 'stim-cleared' = {
            if((CLOCK$time - stim.cleared) >= ISI){
                if(item <= NOF.ITEMS){
                    item = item + 1
                    state = 'show-stim'
                }else{
                    state = 'done'
                }
            }
        }, 'done' = {
            WINDOW$set.visible(F)
            value1 = gui.get.value("", "Słowo 1")
            value2 = gui.get.value("", "Słowo 2")
            WINDOW$set.visible(T)
            WINDOW$clear(c(0, 0, 0))
            WINDOW$display()
            res = list(t1 = t1stim, r1 = value1, t2 = t2stim, r2 = value2)
            return(res)
        })
    }
}

gui.show.instruction("W czasie eksperymentu obowiązuje cisza. Proszę wyłączyć telefon komórkowy. W razie jakichkolwiek wątpliwości proszę nie wołać osoby prowadzącej, tylko podnieść do góry rękę. Osoba prowadząca podejdzie w dogodnym momencie i postara się udzielić wszelkich wyjaśnień. Badanie jest anonimowe.

Za chwilę trzeba będzie wpisać dane osobowe: wiek, płeć oraz pseudonim. Pseudonim składa się z inicjałów (małymi literami, ale bez polskich znaków) oraz czterech cyfr: dnia i miesiąca urodzenia (np.  ms0706).")
gui.user.data()

if(USER.DATA$name != 'admin'){
gui.show.instruction("Teraz rozpocznie się etap polegający na wypełnieniu kilku kwestionariuszy. W każdym z kwestionariuszy prosimy zapoznać się z instrukcją.")

## PANAS-C

gui.show.instruction('Skala, która się za chwilę pojawi, składa się ze słów nazywających różne emocje i uczucia. Przeczytaj każde słowo i zastanów się jak się czujesz ZAZWYCZAJ.')

panas = gui.quest(c('aktywny(a)', '"jak na szpilkach"', 'mocny(a)', 'nerwowy(a)', 'ożywiony(a)', 'pełen (pełna) zapału', 'przerażony(a)', 'raźny(a)', 'silny(a)', 'winny(a)',
            'wystraszony(a)', 'zalękniony(a)', 'zaniepokojony(a)', 'zapalony(a)', 'zawstydzony(a)', 'zdecydowany(a)', 'zdenerwowany(a)', 'zmartwiony(a)', 'żwawy(a)', 'żywy(a)'),
          c('nieznacznie lub wcale', 'trochę', 'umiarkowanie', 'dość mocno', 'bardzo silnie'))

## Ruminacje

gui.show.instruction(
    'Za chwilę pojawi się oto opis szeregu myśli i uczuć, jakimi ludzie reagują na różne sytuacje i zdarzenia. Prosimy zaznaczyć jak często Pana/Panią nachodzi każda z tych myśli lub uczuć - niezależnie od tego, czy same zdarzenia wywołujące te myśli są rzadkie czy częste: interesuje nas częstość samych myśli lub uczuć, a nie częstość zdarzeń, które je wywołują.
')

ruminacje = gui.quest(c(

    'Nie mogę uwolnić się od myśli o różnych niegodziwościach naszego świata.
',
'Wyrzucam sobie nieodpowiedni sposób postępowania w przeszłości.',
'Obawiam się, że świat podąża w coraz gorszym kierunku.',
'"Odtwarzam" sobie myślach własny sposób postępowania w przeszłości.',
'Myślę o zdarzeniach z przeszłości, których bieg chciałbym odmienić.',
'Myślę o tym, jak wiele jest na świecie cierpień niewinnych ludzi.',
'Boli mnie, gdy niektórzy dostają w życiu coś, na co zupełnie sobie nie zasłużyli.',
'Boli mnie, że tak wiele niegodziwych ludzi nigdy nie zostaje ukaranych.',
'Obracam na różne strony w swojej głowie to, co niedawno zrobiłem lub powiedziałem.',
'Smuci mnie, że ludzie myślą tylko o własnym interesie.',
'Boleję nad tym, jakimi egoistami jest większość ludzi.',
'Zastanawiam się dlaczego nie postąpiłem inaczej w pewnych sytuacjach.',
'Boli mnie myśl o tym, że nieuczciwi ludzie się bogacą.',
'Wyobrażam sobie co by było, gdybym inaczej się zachował w pewnych sytuacjach.',
'Myślę o tym, jaki niesprawiedliwy jest świat.',
'Na nowo przeżywam rozczarowania z przeszłości.',
'Powracam myślami do własnych postępków, których już nie można zmienić.',
'Gnębi mnie myśl, jak wiele podłości jest na świecie.',
'Trudno mi zaprzestać myślenia o rzeczach, które mi się nie udały. ',
'Trudno mi opędzić się od myśli, że w pewnych sytuacjach mogłem postąpić inaczej.'),
c('nigdy', 'rzadko', 'czasami', 'często', 'bardzo często'))

## CES-D

gui.show.instruction(
    'Proszę zaznaczyć stwierdzenie, które najlepiej opisuje jak często czuł(a) się Pan/Pani lub zachowywał(a) w ten sposób w ciągu ostatniego tygodnia.
')

cesd = gui.quest(c('Martwiły mnie rzeczy, które zazwyczaj mnie nie martwią.',
            'Nie chciało mi się jeść, nie miałem(am) apetytu.',
            'Czułem(am), że nie mogę pozbyć się chandry, smutku, nawet z pomocą rodziny i przyjaciół.',
            'Wydawało mi się, że jestem gorszym człowiekiem niż inni ludzie.',
            'Miałem(am) trudności ze skoncentrowaniem myśli na tym co robię.',
            'Czułem(am) się przygnębiony(a).',
            'Wszystko, co robiłem(am) przychodziło mi z trudem.',
            'Patrzyłem(am) z nadzieją i ufnością w przyszłość.',
            'Uważałem(am), że moje życie jest nieudane.',
            'Czułem(am) lęk, obawy.',
            'Żle sypiałem(am).',
            'Czułem(am) się szczęśliwy(a).',
            'Byłem(am) bardziej małomówny(a) niż zazwyczaj.',
            'Czułem(am) się samotny(a).',
            'Ludzie odnosili się do mnie nieprzyjaźnie.',
            'Cieszyło mnie życie.',
            'Miałem(am) napady płaczu.',
            'Czułem(am) smutek.',
            'Wydawało mi się, że ludzie mnie nie lubią.',
            'Nic mi nie wychodziło.'),
          c('< 1 dzień', '1-2 dni', '3-4 dni', '5-7 dni'))

## SKU

gui.show.instruction('Prosimy o ustosunkowanie się do podanych poniżej stwierdzeń zakreślając odpowiedź, która najlepiej opisuje Pana/Pani zachowania na co dzień. Przy każdej z pozycji testowych należy  wybrać jedną z odpowiedzi.')

sku = gui.quest(c(
    'Nie mogę skoncentrować się na trudnym zadaniu, kiedy wokół mnie jest hałas.',
    'Jest mi trudno skupić uwagę, kiedy muszę się skoncentrować i rozwiązać jakiś problem.',
    'To co dzieje się wokół rozprasza mnie, nawet jeśli pracuję nad czymś intensywnie.',
    'Potrafię się skoncentrować nawet jeśli w pokoju gra muzyka.',
    'Kiedy staram się skoncentrować, potrafię tak skupić uwagę, że nie zdaję sobie sprawy z tego, co dzieje się wokół mnie.',
    'Łatwo rozpraszają mnie ludzie, którzy rozmawiają w miejscu, gdzie czytam lub się uczę.',
    'Jest mi trudno pozbyć się rozpraszających mnie myśli, gdy staram się skupić na czymś uwagę.',
    'Kiedy jestem czymś podekscytowany/a  jest mi trudno się skoncentrować.',
    'Kiedy próbuję się skupić, nie zwracam uwagi na uczucie głodu i pragnienia.',
    'Potrafię szybko przechodzić z jednego zadania do drugiego.',
    'Potrzebuje trochę czasu, żeby naprawdę  zaangażować się w nowe zadanie.',
    'Jest mi trudno jednocześnie słuchać i robić notatki, np. w czasie wykładu.',
    'Kiedy zajdzie taka potrzeba, potrafię bardzo szybko zainteresować się nowym zagadnieniem.',
    'Mogę z łatwością czytać lub pisać, kiedy rozmawiam przez telefon.',
    'Trudno mi uczestniczyć w dwóch rozmowach jednocześnie.',
    'Kiedy mam szybko wymyślić coś nowego, sprawia mi to trudność.',
    'Potrafię z łatwością skupić uwagę na tym, co robiłem/am nawet, kiedy ktoś mi przeszkodził lub coś mnie  rozproszyło.',
    'Kiedy rozprasza mnie jakaś myśl bez trudu potrafię ją odsunąć od siebie.',
    'Z łatwością mogę wykonywać dwie czynności na przemian.',
    'Jest mi trudno odejść od jednego sposobu myślenia o czymś i spojrzeć na to z innej strony.'),
    c('nigdy', 'czasami', 'często', 'zawsze'))

## Wydarzenia życiowe

gui.show.instruction("Za chwilę pojawi się lista wydarzeń życiowych, które są przykre, trudne, nieprzyjemne.

Jeżeli w okresie ostatnich 3 miesięcy zdarzyło Ci się coś takiego, zaznacz przy nim, jak bardzo było ono przykre, na skali od 1 (mało przykre) do 5 (bardzo przykre).

Jeżeli wymienione zdarzenie nie miało miejsca nie zaznaczaj nic na skali przy nim. Dla ułatwienia zdarzenia połączono w grupy odpowiadające poszczególnym obszarom życia.")

wydarzenia = gui.quest(c('Poważna kłótnia/konflikt w rodzinie',
            'Utrata pracy przez rodzica',
            'Rozwód rodziców',
            'Niesprawiedliwe oskarżenie ze strony rodziców',
            'Patologie/ problemy/ przemoc w rodzinie',
            'Śmierć brata lub siostry',
            'Śmierć matki lub ojca',
            'Poważna zmiana stanu zdrowia lub zachorowanie członka rodziny (choroby, wypadki, zaburzenia zachowania)',
            'Niesprawiedliwa krytyka ze strony nauczyciela/wykładowcy',
            'Nie zdany egzamin/test ',
            'Poważne niepowodzenia w nauce ',
            'Konflikt z nauczycielem/wykładowcą ',
            'Inne kłopoty w nauce',
            'Poważna kłótnia/ konflikt z kolegami',
            'Koledzy/ koleżanki odwrócili się ode mnie',
            'Utrata przyjaciół',
            'Niesprawiedliwa krytyka ze strony kolegów/ koleżanek',
            'Kompromitacja w oczach kolegów',
            'Poważna choroba przyjaciółki/ przyjaciela',
            'Śmierć przyjaciółki/przyjaciela',
            'Poważna choroba',
            'Poważny uraz fizyczny',
            'Problemy zdrowotne',
            'Choroba lub uraz fizyczny, które są mniej poważne',
            'Poważny zabieg dentystyczny',
            'Zdrada ze strony partnera',
            'Poważna kłótnia z chłopakiem/ dziewczyną',
            'Zerwanie z chłopakiem/ dziewczyną',
            'Zawód miłosny',
            'Inne kłopoty z partnerka lub partnerem',
            'Problemy seksualne',
            'Bycie ofiarą gwałtu',
            'Bycie ofiarą napadu lub rabunku',
            'Doświadczenie molestowania seksualnego',
            'Drobny konflikt z prawem',
            'Poważny konflikt z prawem',
            'Pobyt w Izbie wytrzeźwień',
            'Utrata wartościowego przedmiotu',
            'Kłopoty finansowe   ',
            'Udział w wypadku komunikacyjnym',
            'Niechciana ciąża',
            'Śmierć bliskiej osoby',
            'Pożar w domu lub inny kataklizm',
            'Porażka w konkursie lub zawodach',
            'Poważne niepowodzenie w ważnej sprawie'),
          paste(1:5), force.all = F, width = .8)
} ## tylko dla nie-admina

gui.show.instruction("Teraz rozpocznie się zadanie wykrywania słów innego koloru. Zadanie to składa się z serii prób, w trakcie których na ekranie komputera prezentowane są szybko, jedno po drugim, różne słowa. Większość prezentowanych słów ma kolor biały. Dwa spośród tych słów są zielone. Zadanie to polega na napisaniu, po każdej próbie, jakie było pierwsze i jakie było drugie słowo w kolorze zielonym.")

## i = 1:5, żeby liczba prób zgadzała się z procedurą Kostera
run.trials(trial.code, record.session = T,
           expand.grid(t1em = c('neg', 'neu', 'pos'),
                       t1pos = 3:5, t2lag = c(2, 4, 6), i = 1:5),
           condition = 'default')

## Zapisujemy dane kwestionariuszowe
if(USER.DATA$name != 'admin'){
    ## Zapamiętujemy dane kwestionariuszowe
    db.connect()
    panas = as.list(panas)
    names(panas) = paste('i', 1:length(panas), sep = '')
    db.create.data.table(panas, 'ablink_panas')
    panas$session_id = SESSION.ID
    db.insert.data(panas, 'ablink_panas')
    ## ruminacje
    ruminacje = as.list(ruminacje)
    names(ruminacje) = paste('i', 1:length(ruminacje), sep = '')
    db.create.data.table(ruminacje, 'ablink_ruminacje')
    ruminacje$session_id = SESSION.ID
    db.insert.data(ruminacje, 'ablink_ruminacje')
    ## cesd
    cesd = as.list(cesd)
    names(cesd) = paste('i', 1:length(cesd), sep = '')
    db.create.data.table(cesd, 'ablink_cesd')
    cesd$session_id = SESSION.ID
    db.insert.data(cesd, 'ablink_cesd')
    ## sku
    sku = as.list(sku)
    names(sku) = paste('i', 1:length(sku), sep = '')
    db.create.data.table(sku, 'ablink_sku')
    sku$session_id = SESSION.ID
    db.insert.data(sku, 'ablink_sku')
    ## wydarzenia
    wydarzenia = as.list(wydarzenia)
    names(wydarzenia) = paste('i', 1:length(wydarzenia), sep = '')
    db.create.data.table(wydarzenia, 'ablink_wydarzenia')
    wydarzenia$session_id = SESSION.ID
    db.insert.data(wydarzenia, 'ablink_wydarzenia')
    db.disconnect()
}


## Dalszy etap procedury
download.run.task("mcmtest")

gui.show.instruction("Dziękujemy, to już koniec eksperymentu. Prosimy pozostać na miejscu i zaczekać, aż osoba prowadząca badanie poinformuje o dalszym postępowaniu.")

if(!interactive())quit("no")
