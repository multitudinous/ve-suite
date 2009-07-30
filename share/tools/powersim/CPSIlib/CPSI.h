
#ifndef CPSI_H
#define CPSI_H

// --- CPSI Includes --- //
#include "StdAtl.h"

// --- Powersim Includes --- //
#include "PsSimEng.h"

#ifdef CPSI_LIBRARY
#define CPSI_EXPORTS    __declspec( dllexport )
#else
#define CPSI_EXPORTS    __declspec( dllimport )
#endif //CPSI_LIBRARY

namespace cpsi
{

class CPSI_EXPORTS Project
{
public:
    ///Constructor
    Project();

    ///Destructor
    ~Project();

    ///Open a Powersim project file
    ///\param fileName
    void Open(
        const ATL::CString& fileName,
        const ATL::CString& key,
        const ATL::CString& password );

protected:

private:
    ///
    //ATL::CComPtr< ISimulationEngine > m_simulationEngine;

    ///
    //ATL::CComPtr< ISimulationProject > m_simulationProject;

    ///
    //ATL::CComPtr< ISimulation > m_simulation;

};

/*
class CPSI_EXPORTS Variable
{
public:
    ///
    Variable();

    ///
    ~Variable();

protected:

private:


};
*/

} //end cpsi

#endif //CPSI_H
