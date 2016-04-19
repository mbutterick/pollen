<html>
<head>
<meta charset="UTF-8">
<title>◊select['h1 doc] by T. S. Eliot</title>
<link rel="stylesheet" type="text/css" media="all" href="styles.css" />
</head>
<body>◊(->html doc #:splice? #t)
◊(define prev-page (previous here))
◊when/splice[prev-page]{
<div id="prev">← <a href="◊|prev-page|">◊(select 'h1 prev-page)</a></div>}
◊(define next-page (next here))
◊when/splice[next-page]{
<div id="next"><a href="◊|next-page|">◊(select 'h1 next-page)</a> →</div>}
</body>
</html>