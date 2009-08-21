#ifndef FILE_IO_COMMAND_HANDLER_H
#define FILE_IO_COMMAND_HANDLER_H

// --- VE-Suite Includes --- //
#include <ves/open/xml/CommandPtr.h>

// --- C++ Includes --- ///
#include <string>
#include <vector>


namespace vascularsimviewer
{
class vtkUGDataSet;
class VizCommandHandler;

class FileIOCommandHandler
{
    public:
        FileIOCommandHandler();
        ~FileIOCommandHandler();

        //Make FileIO and Viz Handlers aware of each other
        void SetVizCommandHandler( VizCommandHandler* inVizCommandHandler );
        
        void ProcessCurrentCommand( ves::open::xml::CommandPtr command );
        
        void SetActiveDataSetByFilename( const std::string& filename );
        
        vtkUGDataSet* GetActiveDataSet();
        
    private:

        VizCommandHandler* mVizCommandHandler;

        std::vector< vtkUGDataSet* > _DataSets;
        
        vtkUGDataSet* _activeDataSet;

        void LoadVTUFile( const std::string& filename, std::vector< std::string > _arrays );
};

} //end vascularsimviewer

#endif //FILE_IO_COMMAND_HANDLER_H
