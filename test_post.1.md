---
title: "Hello world"
url: "hello_world"
image: "1.jpeg"
time_to_read: "12"
date: "2018-05-10"
abstract: "What would a hello world blog post be without hello world in 10+ different programming languages? A very bad hello world blog post, that's what. Some of these get pretty interesting!"
tags: "hello world,programming,javascript,php,delphi"
---

# HELLO, ${WHO}!



What would a hello world blog post be without hello world in 10+ different programming languages?
A very bad hello world blog post, that's what. Some of these get pretty interesting!  
  
<br/>

##Javascript
Write to the document or to the console, except don't write to the document because
you will regret it!
```javascript
const hello = who => console.log(`hello ${who}`);
//bad
document.write('Hello, world!');
```

##Java
Okay so this one does slightly more than just hello world, it starts a thread on a single threaded thread pool too!
```java{numberLines:true}
public class Bootstrap{
    private static final Executor e = Executors.newSingleThreadedExecutor();

    private static class Greeter{
        public static final helloWorld(){
            return "hello world";
        }
    }

    public static void main(String[] args){
        e.execute(()-> System.out.println(Bootstrap.Greeter.helloWorld()));
    }
}
```

##PHP
```php
 echo "Hello, world";
```

##C sharp
```csharp
using System;
namespace HelloWorld
{
    class Hello 
    {
        static void Main() 
        {
            Console.WriteLine("Hello World!");
        }
    }
}
```

##Python
```python
print 'hello world'
```

##Haskell
```haskell
putStrLn "Hello, world!"
```

## Acronym 
<br/>

> "We build good things with Acronym"

```perl
{{>>{~~~~{-<}~~~~~~~~~{-<-<}}<<}
</(<<<){[<]}:>:{>>{~~~~~~~~{<}~{>}}<<}\
~>{{~{v}}>>>v{~}^<<<}/(<<<){[<<]}:>:{>>{~~~~~~~~{<<}~{>>}}<<}\
~{>>{vvvvvvvv~~~~~~~~~~~~~~~}<<}~{>>{vvvv~~~~~~~~~~~~~}<<}~
{>>{^^^^^^^^^^~}<<}~{>>{v~~~~~~~{{<<}~}v{~}vvvvvvv{~{>>}}^^^^^^~~~
{{<<}~}vvvv{~{>>}}v~~~}<<}~<{{^^^}}~
{>>{vvvvvv~{{<<}~{>>}}^^^^^^^~~~~~~~~~~~{{<<}~{v>}}^^^^^^}}
/{{()}}{[<<<<]}:>:{{~v}}\}
```

##Scala
```scala
object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}
```

##Ruby
```ruby
puts 'Hello, world!'
```


## Pascal

```pascal
program HelloWorld;
begin
  WriteLn('Hello, world!');
end.
```


#html
My favourite!
```html
Hello World!
```

[hello world example](embedded-codesandbox://hello-world-example)
