# train
An endless train simulator, straight from the nightmares and coding interviews

![howto](https://cloud.githubusercontent.com/assets/1123908/19253517/47f1a50a-8f54-11e6-841e-f23c57442b8c.gif)

You find yourself in an endless train (carriges form a loop, you see) and the only way to get out is to count the carriges. You can move around with arrows and switch light with a space. Once you think you've had enough you can press enter and shout the number of carriges. 

As usual this is not your ordinary js game, it only works in linux (windows chokes on a unicode head character fo some reason and I'm going to investigate why). To play it you have to build it: 

    sudo apt-get install haskell-stack
    stack setup
    stack build && stack exec train 

Or, if you're feeling like running binaries from strange people from the internet: [train.zip](https://github.com/reimai/train/files/522905/train.zip) 

Interesting thing: the train _really_ forms a loop. An immutable doubly linked loop. Lazy magic. http://wiki.haskell.org/Tying_the_Knot

