COMPILATION

In order to correctly compile and execute Trail Head, use sbt or anything you want.
The only important thing is that the repository containing the final executable file must also contain:
- the "tools" dossier
- the "cache" dossier
- the "pdf2xml.dtd" file



EXECUTION

When you start the program, after providing the input path, if at that location there isn't any "schedule.xml", an exception will be rised. Since this wasn't part of my tasksand I don't exactly know how this works, I didn't touch at that code part. 
However, the parsing has been completed before the exception.
If you don't want this annoying exception, just comment the code after the parsing process.