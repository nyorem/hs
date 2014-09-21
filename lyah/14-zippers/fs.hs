import Data.List ( break )

x -: f = f x

-- simple filesystem using zippers

type Name = String
type Data = String
-- an item of a filesystem is either a file with name / data or
-- a folder ith a name and a list of items
data FSItem = File Name Data | Folder Name [FSItem] deriving Show

-- example of FS
myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]

-- breadcrumbs for fs:
-- name of the parent folder / files before the focus / files after the focus
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving Show

-- zipper
type FSZipper = (FSItem, [FSCrumb])

-- going up
fsUp :: FSZipper -> FSZipper
fsUp (item, (FSCrumb name ls rs) : bs) = (Folder name (ls ++ [item] ++ rs), bs)

-- going down
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb folderName ls rs : bs)
        where nameIs name (File fileName _) = name == fileName
              nameIs name (Folder folderName _) = name == folderName

-- rename the currently focused file or folder
fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (File name dat, bs) = (File newName dat, bs)
fsRename newName (Folder name content, bs) = (Folder newName content, bs)

-- create a new file / folder in the current folder
fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile newItem (Folder folderName items, bs) = (Folder folderName (newItem:items), bs)

