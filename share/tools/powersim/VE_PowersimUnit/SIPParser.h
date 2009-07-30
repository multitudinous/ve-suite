
#ifndef SIP_PARSER_H
#define SIP_PARSER_H

// --- CPSI Includes --- //
namespace cpsi
{
class Project;
}

// --- C/C++ Includes --- //
#include <string>

class SIPParser
{
public:
    ///Constructor
    SIPParser();

    ///Destructor
    ~SIPParser();

    ///
    void OpenSimulation( const std::string& fileName );

    ///
    void SetWorkingDir( const std::string& workingDir );

protected:

private:
    std::string m_workingDir;

    cpsi::Project* m_powerimProject;

};

#endif //SIP_PARSER_H
