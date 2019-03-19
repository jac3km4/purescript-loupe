module Loupe.DOM where
import Prelude (($), (<$>))
import Loupe.DOM.Props (Props)
import Data.Function (applyFlipped)
import Loupe (Element)
import React.DOM (IsDynamic(..), mkDOM)
import Unsafe.Coerce (unsafeCoerce)

mkElement :: ∀ st act. String -> Array Props -> Array (Element st act) -> Element st act
mkElement el props children conf =
  mkDOM (IsDynamic false) el props $ applyFlipped conf <$> children

text :: ∀ st act. String -> Element st act
text str _ = unsafeCoerce str

a :: ∀ st act. Array Props -> Array (Element st act) -> Element st act
a = mkElement "a"

a' :: ∀ st act. Array (Element st act) -> Element st act
a' = a []

abbr :: ∀ st act. Array Props -> Array (Element st act) -> Element st act
abbr = mkElement "abbr"

abbr' :: ∀ st act. Array (Element st act) -> Element st act
abbr' = abbr []

address :: forall st act. Array Props -> Array (Element st act) -> Element st act
address = mkElement "address"

address' :: forall st act. Array (Element st act) -> Element st act
address' = address []

area :: forall st act. Array Props -> Array (Element st act) -> Element st act
area = mkElement "area"

area' :: forall st act. Array (Element st act) -> Element st act
area' = area []

article :: forall st act. Array Props -> Array (Element st act) -> Element st act
article = mkElement "article"

article' :: forall st act. Array (Element st act) -> Element st act
article' = article []

aside :: forall st act. Array Props -> Array (Element st act) -> Element st act
aside = mkElement "aside"

aside' :: forall st act. Array (Element st act) -> Element st act
aside' = aside []

audio :: forall st act. Array Props -> Array (Element st act) -> Element st act
audio = mkElement "audio"

audio' :: forall st act. Array (Element st act) -> Element st act
audio' = audio []

b :: forall st act. Array Props -> Array (Element st act) -> Element st act
b = mkElement "b"

b' :: forall st act. Array (Element st act) -> Element st act
b' = b []

base :: forall st act. Array Props -> Array (Element st act) -> Element st act
base = mkElement "base"

base' :: forall st act. Array (Element st act) -> Element st act
base' = base []

bdi :: forall st act. Array Props -> Array (Element st act) -> Element st act
bdi = mkElement "bdi"

bdi' :: forall st act. Array (Element st act) -> Element st act
bdi' = bdi []

bdo :: forall st act. Array Props -> Array (Element st act) -> Element st act
bdo = mkElement "bdo"

bdo' :: forall st act. Array (Element st act) -> Element st act
bdo' = bdo []

big :: forall st act. Array Props -> Array (Element st act) -> Element st act
big = mkElement "big"

big' :: forall st act. Array (Element st act) -> Element st act
big' = big []

blockquote :: forall st act. Array Props -> Array (Element st act) -> Element st act
blockquote = mkElement "blockquote"

blockquote' :: forall st act. Array (Element st act) -> Element st act
blockquote' = blockquote []

body :: forall st act. Array Props -> Array (Element st act) -> Element st act
body = mkElement "body"

body' :: forall st act. Array (Element st act) -> Element st act
body' = body []

br :: forall st act. Array Props -> Array (Element st act) -> Element st act
br = mkElement "br"

br' :: forall st act. Array (Element st act) -> Element st act
br' = br []

button :: forall st act. Array Props -> Array (Element st act) -> Element st act
button = mkElement "button"

button' :: forall st act. Array (Element st act) -> Element st act
button' = button []

canvas :: forall st act. Array Props -> Array (Element st act) -> Element st act
canvas = mkElement "canvas"

canvas' :: forall st act. Array (Element st act) -> Element st act
canvas' = canvas []

caption :: forall st act. Array Props -> Array (Element st act) -> Element st act
caption = mkElement "caption"

caption' :: forall st act. Array (Element st act) -> Element st act
caption' = caption []

cite :: forall st act. Array Props -> Array (Element st act) -> Element st act
cite = mkElement "cite"

cite' :: forall st act. Array (Element st act) -> Element st act
cite' = cite []

code :: forall st act. Array Props -> Array (Element st act) -> Element st act
code = mkElement "code"

code' :: forall st act. Array (Element st act) -> Element st act
code' = code []

col :: forall st act. Array Props -> Array (Element st act) -> Element st act
col = mkElement "col"

col' :: forall st act. Array (Element st act) -> Element st act
col' = col []

colgroup :: forall st act. Array Props -> Array (Element st act) -> Element st act
colgroup = mkElement "colgroup"

colgroup' :: forall st act. Array (Element st act) -> Element st act
colgroup' = colgroup []

_data :: forall st act. Array Props -> Array (Element st act) -> Element st act
_data = mkElement "data"

_data' :: forall st act. Array (Element st act) -> Element st act
_data' = _data []

datalist :: forall st act. Array Props -> Array (Element st act) -> Element st act
datalist = mkElement "datalist"

datalist' :: forall st act. Array (Element st act) -> Element st act
datalist' = datalist []

dd :: forall st act. Array Props -> Array (Element st act) -> Element st act
dd = mkElement "dd"

dd' :: forall st act. Array (Element st act) -> Element st act
dd' = dd []

del :: forall st act. Array Props -> Array (Element st act) -> Element st act
del = mkElement "del"

del' :: forall st act. Array (Element st act) -> Element st act
del' = del []

details :: forall st act. Array Props -> Array (Element st act) -> Element st act
details = mkElement "details"

details' :: forall st act. Array (Element st act) -> Element st act
details' = details []

dfn :: forall st act. Array Props -> Array (Element st act) -> Element st act
dfn = mkElement "dfn"

dfn' :: forall st act. Array (Element st act) -> Element st act
dfn' = dfn []

dialog :: forall st act. Array Props -> Array (Element st act) -> Element st act
dialog = mkElement "dialog"

dialog' :: forall st act. Array (Element st act) -> Element st act
dialog' = dialog []

div :: forall st act. Array Props -> Array (Element st act) -> Element st act
div = mkElement "div"

div' :: forall st act. Array (Element st act) -> Element st act
div' = div []

dl :: forall st act. Array Props -> Array (Element st act) -> Element st act
dl = mkElement "dl"

dl' :: forall st act. Array (Element st act) -> Element st act
dl' = dl []

dt :: forall st act. Array Props -> Array (Element st act) -> Element st act
dt = mkElement "dt"

dt' :: forall st act. Array (Element st act) -> Element st act
dt' = dt []

em :: forall st act. Array Props -> Array (Element st act) -> Element st act
em = mkElement "em"

em' :: forall st act. Array (Element st act) -> Element st act
em' = em []

embed :: forall st act. Array Props -> Array (Element st act) -> Element st act
embed = mkElement "embed"

embed' :: forall st act. Array (Element st act) -> Element st act
embed' = embed []

fieldset :: forall st act. Array Props -> Array (Element st act) -> Element st act
fieldset = mkElement "fieldset"

fieldset' :: forall st act. Array (Element st act) -> Element st act
fieldset' = fieldset []

figcaption :: forall st act. Array Props -> Array (Element st act) -> Element st act
figcaption = mkElement "figcaption"

figcaption' :: forall st act. Array (Element st act) -> Element st act
figcaption' = figcaption []

figure :: forall st act. Array Props -> Array (Element st act) -> Element st act
figure = mkElement "figure"

figure' :: forall st act. Array (Element st act) -> Element st act
figure' = figure []

footer :: forall st act. Array Props -> Array (Element st act) -> Element st act
footer = mkElement "footer"

footer' :: forall st act. Array (Element st act) -> Element st act
footer' = footer []

form :: forall st act. Array Props -> Array (Element st act) -> Element st act
form = mkElement "form"

form' :: forall st act. Array (Element st act) -> Element st act
form' = form []

h1 :: forall st act. Array Props -> Array (Element st act) -> Element st act
h1 = mkElement "h1"

h1' :: forall st act. Array (Element st act) -> Element st act
h1' = h1 []

h2 :: forall st act. Array Props -> Array (Element st act) -> Element st act
h2 = mkElement "h2"

h2' :: forall st act. Array (Element st act) -> Element st act
h2' = h2 []

h3 :: forall st act. Array Props -> Array (Element st act) -> Element st act
h3 = mkElement "h3"

h3' :: forall st act. Array (Element st act) -> Element st act
h3' = h3 []

h4 :: forall st act. Array Props -> Array (Element st act) -> Element st act
h4 = mkElement "h4"

h4' :: forall st act. Array (Element st act) -> Element st act
h4' = h4 []

h5 :: forall st act. Array Props -> Array (Element st act) -> Element st act
h5 = mkElement "h5"

h5' :: forall st act. Array (Element st act) -> Element st act
h5' = h5 []

h6 :: forall st act. Array Props -> Array (Element st act) -> Element st act
h6 = mkElement "h6"

h6' :: forall st act. Array (Element st act) -> Element st act
h6' = h6 []

head :: forall st act. Array Props -> Array (Element st act) -> Element st act
head = mkElement "head"

head' :: forall st act. Array (Element st act) -> Element st act
head' = head []

header :: forall st act. Array Props -> Array (Element st act) -> Element st act
header = mkElement "header"

header' :: forall st act. Array (Element st act) -> Element st act
header' = header []

hr :: forall st act. Array Props -> Array (Element st act) -> Element st act
hr = mkElement "hr"

hr' :: forall st act. Array (Element st act) -> Element st act
hr' = hr []

html :: forall st act. Array Props -> Array (Element st act) -> Element st act
html = mkElement "html"

html' :: forall st act. Array (Element st act) -> Element st act
html' = html []

i :: forall st act. Array Props -> Array (Element st act) -> Element st act
i = mkElement "i"

i' :: forall st act. Array (Element st act) -> Element st act
i' = i []

iframe :: forall st act. Array Props -> Array (Element st act) -> Element st act
iframe = mkElement "iframe"

iframe' :: forall st act. Array (Element st act) -> Element st act
iframe' = iframe []

img :: forall st act. Array Props -> Array (Element st act) -> Element st act
img = mkElement "img"

img' :: forall st act. Array (Element st act) -> Element st act
img' = img []

input :: forall st act. Array Props -> Array (Element st act) -> Element st act
input = mkElement "input"

input' :: forall st act. Array (Element st act) -> Element st act
input' = input []

ins :: forall st act. Array Props -> Array (Element st act) -> Element st act
ins = mkElement "ins"

ins' :: forall st act. Array (Element st act) -> Element st act
ins' = ins []

kbd :: forall st act. Array Props -> Array (Element st act) -> Element st act
kbd = mkElement "kbd"

kbd' :: forall st act. Array (Element st act) -> Element st act
kbd' = kbd []

keygen :: forall st act. Array Props -> Array (Element st act) -> Element st act
keygen = mkElement "keygen"

keygen' :: forall st act. Array (Element st act) -> Element st act
keygen' = keygen []

label :: forall st act. Array Props -> Array (Element st act) -> Element st act
label = mkElement "label"

label' :: forall st act. Array (Element st act) -> Element st act
label' = label []

legend :: forall st act. Array Props -> Array (Element st act) -> Element st act
legend = mkElement "legend"

legend' :: forall st act. Array (Element st act) -> Element st act
legend' = legend []

li :: forall st act. Array Props -> Array (Element st act) -> Element st act
li = mkElement "li"

li' :: forall st act. Array (Element st act) -> Element st act
li' = li []

link :: forall st act. Array Props -> Array (Element st act) -> Element st act
link = mkElement "link"

link' :: forall st act. Array (Element st act) -> Element st act
link' = body []

main :: forall st act. Array Props -> Array (Element st act) -> Element st act
main = mkElement "main"

main' :: forall st act. Array (Element st act) -> Element st act
main' = main []

map :: forall st act. Array Props -> Array (Element st act) -> Element st act
map = mkElement "map"

map' :: forall st act. Array (Element st act) -> Element st act
map' = map []

mark :: forall st act. Array Props -> Array (Element st act) -> Element st act
mark = mkElement "mark"

mark' :: forall st act. Array (Element st act) -> Element st act
mark' = mark []

menu :: forall st act. Array Props -> Array (Element st act) -> Element st act
menu = mkElement "menu"

menu' :: forall st act. Array (Element st act) -> Element st act
menu' = menu []

menuitem :: forall st act. Array Props -> Array (Element st act) -> Element st act
menuitem = mkElement "menuitem"

menuitem' :: forall st act. Array (Element st act) -> Element st act
menuitem' = menuitem []

meta :: forall st act. Array Props -> Array (Element st act) -> Element st act
meta = mkElement "meta"

meta' :: forall st act. Array (Element st act) -> Element st act
meta' = meta []

meter :: forall st act. Array Props -> Array (Element st act) -> Element st act
meter = mkElement "meter"

meter' :: forall st act. Array (Element st act) -> Element st act
meter' = meter []

nav :: forall st act. Array Props -> Array (Element st act) -> Element st act
nav = mkElement "nav"

nav' :: forall st act. Array (Element st act) -> Element st act
nav' = nav []

noscript :: forall st act. Array Props -> Array (Element st act) -> Element st act
noscript = mkElement "noscript"

noscript' :: forall st act. Array (Element st act) -> Element st act
noscript' = noscript []

object :: forall st act. Array Props -> Array (Element st act) -> Element st act
object = mkElement "object"

object' :: forall st act. Array (Element st act) -> Element st act
object' = object []

ol :: forall st act. Array Props -> Array (Element st act) -> Element st act
ol = mkElement "ol"

ol' :: forall st act. Array (Element st act) -> Element st act
ol' = ol []

optgroup :: forall st act. Array Props -> Array (Element st act) -> Element st act
optgroup = mkElement "optgroup"

optgroup' :: forall st act. Array (Element st act) -> Element st act
optgroup' = optgroup []

option :: forall st act. Array Props -> Array (Element st act) -> Element st act
option = mkElement "option"

option' :: forall st act. Array (Element st act) -> Element st act
option' = option []

output :: forall st act. Array Props -> Array (Element st act) -> Element st act
output = mkElement "output"

output' :: forall st act. Array (Element st act) -> Element st act
output' = output []

p :: forall st act. Array Props -> Array (Element st act) -> Element st act
p = mkElement "p"

p' :: forall st act. Array (Element st act) -> Element st act
p' = p []

param :: forall st act. Array Props -> Array (Element st act) -> Element st act
param = mkElement "param"

param' :: forall st act. Array (Element st act) -> Element st act
param' = param []

picture :: forall st act. Array Props -> Array (Element st act) -> Element st act
picture = mkElement "picture"

picture' :: forall st act. Array (Element st act) -> Element st act
picture' = picture []

pre :: forall st act. Array Props -> Array (Element st act) -> Element st act
pre = mkElement "pre"

pre' :: forall st act. Array (Element st act) -> Element st act
pre' = pre []

progress :: forall st act. Array Props -> Array (Element st act) -> Element st act
progress = mkElement "progress"

progress' :: forall st act. Array (Element st act) -> Element st act
progress' = progress []

q :: forall st act. Array Props -> Array (Element st act) -> Element st act
q = mkElement "q"

q' :: forall st act. Array (Element st act) -> Element st act
q' = q []

rp :: forall st act. Array Props -> Array (Element st act) -> Element st act
rp = mkElement "rp"

rp' :: forall st act. Array (Element st act) -> Element st act
rp' = rp []

rt :: forall st act. Array Props -> Array (Element st act) -> Element st act
rt = mkElement "rt"

rt' :: forall st act. Array (Element st act) -> Element st act
rt' = rt []

ruby :: forall st act. Array Props -> Array (Element st act) -> Element st act
ruby = mkElement "ruby"

ruby' :: forall st act. Array (Element st act) -> Element st act
ruby' = ruby []

s :: forall st act. Array Props -> Array (Element st act) -> Element st act
s = mkElement "s"

s' :: forall st act. Array (Element st act) -> Element st act
s' = s []

samp :: forall st act. Array Props -> Array (Element st act) -> Element st act
samp = mkElement "samp"

samp' :: forall st act. Array (Element st act) -> Element st act
samp' = samp []

script :: forall st act. Array Props -> Array (Element st act) -> Element st act
script = mkElement "script"

script' :: forall st act. Array (Element st act) -> Element st act
script' = script []

section :: forall st act. Array Props -> Array (Element st act) -> Element st act
section = mkElement "section"

section' :: forall st act. Array (Element st act) -> Element st act
section' = section []

select :: forall st act. Array Props -> Array (Element st act) -> Element st act
select = mkElement "select"

select' :: forall st act. Array (Element st act) -> Element st act
select' = select []

small :: forall st act. Array Props -> Array (Element st act) -> Element st act
small = mkElement "small"

small' :: forall st act. Array (Element st act) -> Element st act
small' = small []

source :: forall st act. Array Props -> Array (Element st act) -> Element st act
source = mkElement "source"

source' :: forall st act. Array (Element st act) -> Element st act
source' = source []

span :: forall st act. Array Props -> Array (Element st act) -> Element st act
span = mkElement "span"

span' :: forall st act. Array (Element st act) -> Element st act
span' = span []

strong :: forall st act. Array Props -> Array (Element st act) -> Element st act
strong = mkElement "strong"

strong' :: forall st act. Array (Element st act) -> Element st act
strong' = strong []

style :: forall st act. Array Props -> Array (Element st act) -> Element st act
style = mkElement "style"

style' :: forall st act. Array (Element st act) -> Element st act
style' = style []

sub :: forall st act. Array Props -> Array (Element st act) -> Element st act
sub = mkElement "sub"

sub' :: forall st act. Array (Element st act) -> Element st act
sub' = sub []

summary :: forall st act. Array Props -> Array (Element st act) -> Element st act
summary = mkElement "summary"

summary' :: forall st act. Array (Element st act) -> Element st act
summary' = summary []

sup :: forall st act. Array Props -> Array (Element st act) -> Element st act
sup = mkElement "sup"

sup' :: forall st act. Array (Element st act) -> Element st act
sup' = sup []

table :: forall st act. Array Props -> Array (Element st act) -> Element st act
table = mkElement "table"

table' :: forall st act. Array (Element st act) -> Element st act
table' = table []

tbody :: forall st act. Array Props -> Array (Element st act) -> Element st act
tbody = mkElement "tbody"

tbody' :: forall st act. Array (Element st act) -> Element st act
tbody' = tbody []

td :: forall st act. Array Props -> Array (Element st act) -> Element st act
td = mkElement "td"

td' :: forall st act. Array (Element st act) -> Element st act
td' = td []

textarea :: forall st act. Array Props -> Array (Element st act) -> Element st act
textarea = mkElement "textarea"

textarea' :: forall st act. Array (Element st act) -> Element st act
textarea' = textarea []

tfoot :: forall st act. Array Props -> Array (Element st act) -> Element st act
tfoot = mkElement "tfoot"

tfoot' :: forall st act. Array (Element st act) -> Element st act
tfoot' = tfoot []

th :: forall st act. Array Props -> Array (Element st act) -> Element st act
th = mkElement "th"

th' :: forall st act. Array (Element st act) -> Element st act
th' = th []

thead :: forall st act. Array Props -> Array (Element st act) -> Element st act
thead = mkElement "thead"

thead' :: forall st act. Array (Element st act) -> Element st act
thead' = thead []

time :: forall st act. Array Props -> Array (Element st act) -> Element st act
time = mkElement "time"

time' :: forall st act. Array (Element st act) -> Element st act
time' = time []

title :: forall st act. Array Props -> Array (Element st act) -> Element st act
title = mkElement "title"

title' :: forall st act. Array (Element st act) -> Element st act
title' = title []

tr :: forall st act. Array Props -> Array (Element st act) -> Element st act
tr = mkElement "tr"

tr' :: forall st act. Array (Element st act) -> Element st act
tr' = tr []

track :: forall st act. Array Props -> Array (Element st act) -> Element st act
track = mkElement "track"

track' :: forall st act. Array (Element st act) -> Element st act
track' = track []

u :: forall st act. Array Props -> Array (Element st act) -> Element st act
u = mkElement "u"

u' :: forall st act. Array (Element st act) -> Element st act
u' = u []

ul :: forall st act. Array Props -> Array (Element st act) -> Element st act
ul = mkElement "ul"

ul' :: forall st act. Array (Element st act) -> Element st act
ul' = ul []

var :: forall st act. Array Props -> Array (Element st act) -> Element st act
var = mkElement "var"

var' :: forall st act. Array (Element st act) -> Element st act
var' = var []

video :: forall st act. Array Props -> Array (Element st act) -> Element st act
video = mkElement "video"

video' :: forall st act. Array (Element st act) -> Element st act
video' = video []

wbr :: forall st act. Array Props -> Array (Element st act) -> Element st act
wbr = mkElement "body"

wbr' :: forall st act. Array (Element st act) -> Element st act
wbr' = wbr []
