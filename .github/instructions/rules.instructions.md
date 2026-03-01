--- 
applyTo: '**'
---

!!!IMPORTANT NOTE: Sometimes VS Code will not update to changes in the dll. After a fix, always defer to the result of a build even if the Editor claims a possible compile error, always then check with and defer to the build result!!!

Do NOT directly update *FBS.cs C# files. These are generated from the .fbs schema files. Instead, update the .fbs files building the .sln will update the C#.