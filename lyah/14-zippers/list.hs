-- Zippers for lists

-- | Focus = list
-- | Breadcrumbs = list
type ListZipper a = ([a], [a])

-- | Going forward.
goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

-- | Going backward.
goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

-- Zippers can be used for text editors:
-- file = [String]
-- current line = String which is a focus
