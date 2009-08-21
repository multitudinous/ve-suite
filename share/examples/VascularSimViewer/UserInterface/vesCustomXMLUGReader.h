#ifndef VES_CUSTOM_XML_UG_READER_H
#define VES_CUSTOM_XML_UG_READER_H

// --- vtk Includes --- //
#include <vtkXMLUnstructuredGridReader.h>

namespace vascularsimviewer
{

class VTK_IO_EXPORT vesCustomXMLUGReader : public vtkXMLUnstructuredGridReader
{
    public:
        //Constructor
        vesCustomXMLUGReader();
				
        //Destructor
        ~vesCustomXMLUGReader();
        
        int GetNumberofPointArrays();
	
    private:
    

};
}  //end vascularsimviewer namespace

#endif // VES_CUSTOM_XML_UG_READER_H
