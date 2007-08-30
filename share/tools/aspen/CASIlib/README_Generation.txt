Basically, follow the method in this link to run midl.exe come with VC

http://www.codeproject.com/com/vb_from_vc.asp

Not sure where the OLE view for VC2005 is.

Yang


To create the stub code utilized by the CASI library please follow the above 
instructions. 

For use with Visual Studio 2005

Steps:

1. View -> Object Browser
2. Browse -> Edit custom component set
3. Add "Aspen Plus GUI 20.0 Type Library" to the project
4. You will see a Happ node in the object viewer
5. 

Generating IDL:

1. Start->MS Visual Studio 2005->MS Visual Studio Tools-> MS Visual Studio Command Prompt
2. midl
3. 