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
3. midl happ.idl
4. I then had to move a bunch of enums up to the beginning of the file to resolve errors
5. I then got redefinition errors which i resovled by removing the interface definitions fro mthe idl file.

Get oleview in Visual Studio 2005

1. Start->MS Visual Studio 2005->MS Visual Studio Tools-> MS Visual Studio Command Prompt
2. oleview
3. open type library
4. double click "Aspen Plus GUI 20.0 Type Library"
5. Save the idl interface out to a file

