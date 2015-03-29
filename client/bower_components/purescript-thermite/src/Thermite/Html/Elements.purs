module Thermite.Html.Elements where
    
import Thermite.Types
import Thermite.Internal
    
a :: forall action. Props action -> [Html action] -> Html action
a = createElementImpl "a"

a' :: forall action. [Html action] -> Html action
a' = a []

abbr :: forall action. Props action -> [Html action] -> Html action
abbr = createElementImpl "abbr"

abbr' :: forall action. [Html action] -> Html action
abbr' = abbr []

address :: forall action. Props action -> [Html action] -> Html action
address = createElementImpl "address"

address' :: forall action. [Html action] -> Html action
address' = address []

area :: forall action. Props action -> [Html action] -> Html action
area = createElementImpl "area"

area' :: forall action. [Html action] -> Html action
area' = area []

article :: forall action. Props action -> [Html action] -> Html action
article = createElementImpl "article"

article' :: forall action. [Html action] -> Html action
article' = article []

aside :: forall action. Props action -> [Html action] -> Html action
aside = createElementImpl "aside"

aside' :: forall action. [Html action] -> Html action
aside' = aside []

audio :: forall action. Props action -> [Html action] -> Html action
audio = createElementImpl "audio"

audio' :: forall action. [Html action] -> Html action
audio' = audio []

b :: forall action. Props action -> [Html action] -> Html action
b = createElementImpl "b"

b' :: forall action. [Html action] -> Html action
b' = b []

base :: forall action. Props action -> [Html action] -> Html action
base = createElementImpl "base"

base' :: forall action. [Html action] -> Html action
base' = base []

bdi :: forall action. Props action -> [Html action] -> Html action
bdi = createElementImpl "bdi"

bdi' :: forall action. [Html action] -> Html action
bdi' = bdi []

bdo :: forall action. Props action -> [Html action] -> Html action
bdo = createElementImpl "bdo"

bdo' :: forall action. [Html action] -> Html action
bdo' = bdo []

big :: forall action. Props action -> [Html action] -> Html action
big = createElementImpl "big"

big' :: forall action. [Html action] -> Html action
big' = big []

blockquote :: forall action. Props action -> [Html action] -> Html action
blockquote = createElementImpl "blockquote"

blockquote' :: forall action. [Html action] -> Html action
blockquote' = blockquote []

body :: forall action. Props action -> [Html action] -> Html action
body = createElementImpl "body"

body' :: forall action. [Html action] -> Html action
body' = body []

br :: forall action. Props action -> [Html action] -> Html action
br = createElementImpl "br"

br' :: forall action. [Html action] -> Html action
br' = br []

button :: forall action. Props action -> [Html action] -> Html action
button = createElementImpl "button"

button' :: forall action. [Html action] -> Html action
button' = button []

canvas :: forall action. Props action -> [Html action] -> Html action
canvas = createElementImpl "canvas"

canvas' :: forall action. [Html action] -> Html action
canvas' = canvas []

caption :: forall action. Props action -> [Html action] -> Html action
caption = createElementImpl "caption"

caption' :: forall action. [Html action] -> Html action
caption' = caption []

cite :: forall action. Props action -> [Html action] -> Html action
cite = createElementImpl "cite"

cite' :: forall action. [Html action] -> Html action
cite' = cite []

code :: forall action. Props action -> [Html action] -> Html action
code = createElementImpl "code"

code' :: forall action. [Html action] -> Html action
code' = code []

col :: forall action. Props action -> [Html action] -> Html action
col = createElementImpl "col"

col' :: forall action. [Html action] -> Html action
col' = col []

colgroup :: forall action. Props action -> [Html action] -> Html action
colgroup = createElementImpl "colgroup"

colgroup' :: forall action. [Html action] -> Html action
colgroup' = colgroup []

_data :: forall action. Props action -> [Html action] -> Html action
_data = createElementImpl "data"

_data' :: forall action. [Html action] -> Html action
_data' = _data []

datalist :: forall action. Props action -> [Html action] -> Html action
datalist = createElementImpl "datalist"

datalist' :: forall action. [Html action] -> Html action
datalist' = datalist []

dd :: forall action. Props action -> [Html action] -> Html action
dd = createElementImpl "dd"

dd' :: forall action. [Html action] -> Html action
dd' = dd []

del :: forall action. Props action -> [Html action] -> Html action
del = createElementImpl "del"

del' :: forall action. [Html action] -> Html action
del' = del []

details :: forall action. Props action -> [Html action] -> Html action
details = createElementImpl "details"

details' :: forall action. [Html action] -> Html action
details' = details []

dfn :: forall action. Props action -> [Html action] -> Html action
dfn = createElementImpl "dfn"

dfn' :: forall action. [Html action] -> Html action
dfn' = dfn []

dialog :: forall action. Props action -> [Html action] -> Html action
dialog = createElementImpl "dialog"

dialog' :: forall action. [Html action] -> Html action
dialog' = dialog []

div :: forall action. Props action -> [Html action] -> Html action
div = createElementImpl "div"

div' :: forall action. [Html action] -> Html action
div' = div []

dl :: forall action. Props action -> [Html action] -> Html action
dl = createElementImpl "dl"

dl' :: forall action. [Html action] -> Html action
dl' = dl []

dt :: forall action. Props action -> [Html action] -> Html action
dt = createElementImpl "dt"

dt' :: forall action. [Html action] -> Html action
dt' = dt []

em :: forall action. Props action -> [Html action] -> Html action
em = createElementImpl "em"

em' :: forall action. [Html action] -> Html action
em' = em []

embed :: forall action. Props action -> [Html action] -> Html action
embed = createElementImpl "embed"

embed' :: forall action. [Html action] -> Html action
embed' = embed []

fieldset :: forall action. Props action -> [Html action] -> Html action
fieldset = createElementImpl "fieldset"

fieldset' :: forall action. [Html action] -> Html action
fieldset' = fieldset []

figcaption :: forall action. Props action -> [Html action] -> Html action
figcaption = createElementImpl "figcaption"

figcaption' :: forall action. [Html action] -> Html action
figcaption' = figcaption []

figure :: forall action. Props action -> [Html action] -> Html action
figure = createElementImpl "figure"

figure' :: forall action. [Html action] -> Html action
figure' = figure []

footer :: forall action. Props action -> [Html action] -> Html action
footer = createElementImpl "footer"

footer' :: forall action. [Html action] -> Html action
footer' = footer []

form :: forall action. Props action -> [Html action] -> Html action
form = createElementImpl "form"

form' :: forall action. [Html action] -> Html action
form' = form []

h1 :: forall action. Props action -> [Html action] -> Html action
h1 = createElementImpl "h1"

h1' :: forall action. [Html action] -> Html action
h1' = h1 []

h2 :: forall action. Props action -> [Html action] -> Html action
h2 = createElementImpl "h2"

h2' :: forall action. [Html action] -> Html action
h2' = h2 []

h3 :: forall action. Props action -> [Html action] -> Html action
h3 = createElementImpl "h3"

h3' :: forall action. [Html action] -> Html action
h3' = h3 []

h4 :: forall action. Props action -> [Html action] -> Html action
h4 = createElementImpl "h4"

h4' :: forall action. [Html action] -> Html action
h4' = h4 []

h5 :: forall action. Props action -> [Html action] -> Html action
h5 = createElementImpl "h5"

h5' :: forall action. [Html action] -> Html action
h5' = h5 []

h6 :: forall action. Props action -> [Html action] -> Html action
h6 = createElementImpl "h6"

h6' :: forall action. [Html action] -> Html action
h6' = h6 []

head :: forall action. Props action -> [Html action] -> Html action
head = createElementImpl "head"

head' :: forall action. [Html action] -> Html action
head' = head []

header :: forall action. Props action -> [Html action] -> Html action
header = createElementImpl "header"

header' :: forall action. [Html action] -> Html action
header' = header []

hr :: forall action. Props action -> [Html action] -> Html action
hr = createElementImpl "hr"

hr' :: forall action. [Html action] -> Html action
hr' = hr []

html :: forall action. Props action -> [Html action] -> Html action
html = createElementImpl "html"

html' :: forall action. [Html action] -> Html action
html' = html []

i :: forall action. Props action -> [Html action] -> Html action
i = createElementImpl "i"

i' :: forall action. [Html action] -> Html action
i' = i []

iframe :: forall action. Props action -> [Html action] -> Html action
iframe = createElementImpl "iframe"

iframe' :: forall action. [Html action] -> Html action
iframe' = iframe []

img :: forall action. Props action -> [Html action] -> Html action
img = createElementImpl "img"

img' :: forall action. [Html action] -> Html action
img' = img []

input :: forall action. Props action -> [Html action] -> Html action
input = createElementImpl "input"

input' :: forall action. [Html action] -> Html action
input' = input []

ins :: forall action. Props action -> [Html action] -> Html action
ins = createElementImpl "ins"

ins' :: forall action. [Html action] -> Html action
ins' = ins []

kbd :: forall action. Props action -> [Html action] -> Html action
kbd = createElementImpl "kbd"

kbd' :: forall action. [Html action] -> Html action
kbd' = kbd []

keygen :: forall action. Props action -> [Html action] -> Html action
keygen = createElementImpl "keygen"

keygen' :: forall action. [Html action] -> Html action
keygen' = keygen []

label :: forall action. Props action -> [Html action] -> Html action
label = createElementImpl "label"

label' :: forall action. [Html action] -> Html action
label' = label []

legend :: forall action. Props action -> [Html action] -> Html action
legend = createElementImpl "legend"

legend' :: forall action. [Html action] -> Html action
legend' = legend []

li :: forall action. Props action -> [Html action] -> Html action
li = createElementImpl "li"

li' :: forall action. [Html action] -> Html action
li' = li []

link :: forall action. Props action -> [Html action] -> Html action
link = createElementImpl "link"

link' :: forall action. [Html action] -> Html action
link' = body []

main :: forall action. Props action -> [Html action] -> Html action
main = createElementImpl "main"

main' :: forall action. [Html action] -> Html action
main' = main []

map :: forall action. Props action -> [Html action] -> Html action
map = createElementImpl "map"

map' :: forall action. [Html action] -> Html action
map' = map []

mark :: forall action. Props action -> [Html action] -> Html action
mark = createElementImpl "mark"

mark' :: forall action. [Html action] -> Html action
mark' = mark []

menu :: forall action. Props action -> [Html action] -> Html action
menu = createElementImpl "menu"

menu' :: forall action. [Html action] -> Html action
menu' = menu []

menuitem :: forall action. Props action -> [Html action] -> Html action
menuitem = createElementImpl "menuitem"

menuitem' :: forall action. [Html action] -> Html action
menuitem' = menuitem []

meta :: forall action. Props action -> [Html action] -> Html action
meta = createElementImpl "meta"

meta' :: forall action. [Html action] -> Html action
meta' = meta []

meter :: forall action. Props action -> [Html action] -> Html action
meter = createElementImpl "meter"

meter' :: forall action. [Html action] -> Html action
meter' = meter []

nav :: forall action. Props action -> [Html action] -> Html action
nav = createElementImpl "nav"

nav' :: forall action. [Html action] -> Html action
nav' = nav []

noscript :: forall action. Props action -> [Html action] -> Html action
noscript = createElementImpl "noscript"

noscript' :: forall action. [Html action] -> Html action
noscript' = noscript []

object :: forall action. Props action -> [Html action] -> Html action
object = createElementImpl "object"

object' :: forall action. [Html action] -> Html action
object' = object []

ol :: forall action. Props action -> [Html action] -> Html action
ol = createElementImpl "ol"

ol' :: forall action. [Html action] -> Html action
ol' = ol []

optgroup :: forall action. Props action -> [Html action] -> Html action
optgroup = createElementImpl "optgroup"

optgroup' :: forall action. [Html action] -> Html action
optgroup' = optgroup []

option :: forall action. Props action -> [Html action] -> Html action
option = createElementImpl "option"

option' :: forall action. [Html action] -> Html action
option' = option []

output :: forall action. Props action -> [Html action] -> Html action
output = createElementImpl "output"

output' :: forall action. [Html action] -> Html action
output' = output []

p :: forall action. Props action -> [Html action] -> Html action
p = createElementImpl "p"

p' :: forall action. [Html action] -> Html action
p' = p []

param :: forall action. Props action -> [Html action] -> Html action
param = createElementImpl "param"

param' :: forall action. [Html action] -> Html action
param' = param []

picture :: forall action. Props action -> [Html action] -> Html action
picture = createElementImpl "picture"

picture' :: forall action. [Html action] -> Html action
picture' = picture []

pre :: forall action. Props action -> [Html action] -> Html action
pre = createElementImpl "pre"

pre' :: forall action. [Html action] -> Html action
pre' = pre []

progress :: forall action. Props action -> [Html action] -> Html action
progress = createElementImpl "progress"

progress' :: forall action. [Html action] -> Html action
progress' = progress []

q :: forall action. Props action -> [Html action] -> Html action
q = createElementImpl "q"

q' :: forall action. [Html action] -> Html action
q' = q []

rp :: forall action. Props action -> [Html action] -> Html action
rp = createElementImpl "rp"

rp' :: forall action. [Html action] -> Html action
rp' = rp []

rt :: forall action. Props action -> [Html action] -> Html action
rt = createElementImpl "rt"

rt' :: forall action. [Html action] -> Html action
rt' = rt []

ruby :: forall action. Props action -> [Html action] -> Html action
ruby = createElementImpl "ruby"

ruby' :: forall action. [Html action] -> Html action
ruby' = ruby []

s :: forall action. Props action -> [Html action] -> Html action
s = createElementImpl "s"

s' :: forall action. [Html action] -> Html action
s' = s []

samp :: forall action. Props action -> [Html action] -> Html action
samp = createElementImpl "samp"

samp' :: forall action. [Html action] -> Html action
samp' = samp []

script :: forall action. Props action -> [Html action] -> Html action
script = createElementImpl "script"

script' :: forall action. [Html action] -> Html action
script' = script []

section :: forall action. Props action -> [Html action] -> Html action
section = createElementImpl "section"

section' :: forall action. [Html action] -> Html action
section' = section []

select :: forall action. Props action -> [Html action] -> Html action
select = createElementImpl "select"

select' :: forall action. [Html action] -> Html action
select' = select []

small :: forall action. Props action -> [Html action] -> Html action
small = createElementImpl "small"

small' :: forall action. [Html action] -> Html action
small' = small []

source :: forall action. Props action -> [Html action] -> Html action
source = createElementImpl "source"

source' :: forall action. [Html action] -> Html action
source' = source []

span :: forall action. Props action -> [Html action] -> Html action
span = createElementImpl "span"

span' :: forall action. [Html action] -> Html action
span' = span []

strong :: forall action. Props action -> [Html action] -> Html action
strong = createElementImpl "strong"

strong' :: forall action. [Html action] -> Html action
strong' = strong []

style :: forall action. Props action -> [Html action] -> Html action
style = createElementImpl "style"

style' :: forall action. [Html action] -> Html action
style' = style []

sub :: forall action. Props action -> [Html action] -> Html action
sub = createElementImpl "sub"

sub' :: forall action. [Html action] -> Html action
sub' = sub []

summary :: forall action. Props action -> [Html action] -> Html action
summary = createElementImpl "summary"

summary' :: forall action. [Html action] -> Html action
summary' = summary []

sup :: forall action. Props action -> [Html action] -> Html action
sup = createElementImpl "sup"

sup' :: forall action. [Html action] -> Html action
sup' = sup []

table :: forall action. Props action -> [Html action] -> Html action
table = createElementImpl "table"

table' :: forall action. [Html action] -> Html action
table' = table []

tbody :: forall action. Props action -> [Html action] -> Html action
tbody = createElementImpl "tbody"

tbody' :: forall action. [Html action] -> Html action
tbody' = tbody []

td :: forall action. Props action -> [Html action] -> Html action
td = createElementImpl "td"

td' :: forall action. [Html action] -> Html action
td' = td []

textarea :: forall action. Props action -> [Html action] -> Html action
textarea = createElementImpl "textarea"

textarea' :: forall action. [Html action] -> Html action
textarea' = textarea []

tfoot :: forall action. Props action -> [Html action] -> Html action
tfoot = createElementImpl "tfoot"

tfoot' :: forall action. [Html action] -> Html action
tfoot' = tfoot []

th :: forall action. Props action -> [Html action] -> Html action
th = createElementImpl "th"

th' :: forall action. [Html action] -> Html action
th' = th []

thead :: forall action. Props action -> [Html action] -> Html action
thead = createElementImpl "thead"

thead' :: forall action. [Html action] -> Html action
thead' = thead []

time :: forall action. Props action -> [Html action] -> Html action
time = createElementImpl "time"

time' :: forall action. [Html action] -> Html action
time' = time []

title :: forall action. Props action -> [Html action] -> Html action
title = createElementImpl "title"

title' :: forall action. [Html action] -> Html action
title' = title []

tr :: forall action. Props action -> [Html action] -> Html action
tr = createElementImpl "tr"

tr' :: forall action. [Html action] -> Html action
tr' = tr []

track :: forall action. Props action -> [Html action] -> Html action
track = createElementImpl "track"

track' :: forall action. [Html action] -> Html action
track' = track []

u :: forall action. Props action -> [Html action] -> Html action
u = createElementImpl "u"

u' :: forall action. [Html action] -> Html action
u' = u []

ul :: forall action. Props action -> [Html action] -> Html action
ul = createElementImpl "ul"

ul' :: forall action. [Html action] -> Html action
ul' = ul []

var :: forall action. Props action -> [Html action] -> Html action
var = createElementImpl "var"

var' :: forall action. [Html action] -> Html action
var' = var []

video :: forall action. Props action -> [Html action] -> Html action
video = createElementImpl "video"

video' :: forall action. [Html action] -> Html action
video' = video []

wbr :: forall action. Props action -> [Html action] -> Html action
wbr = createElementImpl "body"

wbr' :: forall action. [Html action] -> Html action
wbr' = wbr []
