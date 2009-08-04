
#ifndef POWERSIM_UNIT_I_H
#define POWERSIM_UNIT_I_H

// --- VE_PowersimUnit Includes --- //
class CMainDlg;
class CorbaUnitManager;
class SIPParser;

// --- VE-Suite Includes --- //
#include <ves/open/moduleS.h>
#include <ves/open/xml/CommandPtr.h>

// --- C/C++ Includes --- //
#include <set>
#include <string>

class  Body_Unit_i : public virtual POA_Body::Unit
{
public:
    ///Constructor
    Body_Unit_i(
        const std::string& unitName,
        CMainDlg* mainDialog,
        CorbaUnitManager* corbaUnitManager,
        const std::string& workingDir );

    ///Destructor
    virtual ~Body_Unit_i();

    ///
    virtual void StartCalc( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    ///
    virtual void StopCalc( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    ///
    virtual void PauseCalc( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    ///
    virtual void Resume( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    ///
    virtual char* GetStatusMessage( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    ///
    virtual char* GetUserData( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    ///
    virtual void SetParams( CORBA::Long id, const char* param )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    ///
    virtual void SetID( CORBA::Long id )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    ///
    virtual void SetCurID( CORBA::Long id )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    ///
    virtual Types::ArrayLong* GetID( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    ///
    virtual CORBA::Long GetCurID( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );  

    ///
    virtual void SetName( const char* name )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    ///
    virtual char* GetName( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    ///
    virtual char* Query( const char* commands )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    ///
    virtual void DeleteModuleInstance( CORBA::Long module_id )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    //void ShowAspen();
    //void HideAspen();
    //void CloseAspen();
    //void ReinitializeAspen();
    //void SaveAspen( std::string filename );
    //void StepSim();

    ///
    char* HandleGetNetwork( ves::open::xml::CommandPtr cmd );

    ///
    char* HandleOpenSimulation( ves::open::xml::CommandPtr cmd );
    /*
    void ReinitializeBlock(ves::open::xml::CommandPtr cmd);
    char* handleSaveAs(ves::open::xml::CommandPtr cmd);
    char* handleGetModuleParamList(ves::open::xml::CommandPtr cmd);
    char* handleGetInputModuleParamList(ves::open::xml::CommandPtr cmd);
    char* handleGetInputModuleProperties(ves::open::xml::CommandPtr cmd);
    char* handleGetOutputModuleParamList(ves::open::xml::CommandPtr cmd);
    char* handleGetOutputModuleProperties(ves::open::xml::CommandPtr cmd);
    char* handleGetStreamModuleParamList(ves::open::xml::CommandPtr cmd);
    char* handleGetStreamInputModuleParamList(ves::open::xml::CommandPtr cmd);
    char* handleGetStreamInputModuleProperties(ves::open::xml::CommandPtr cmd);
    char* handleGetStreamOutputModuleParamList(ves::open::xml::CommandPtr cmd);
    char* handleGetStreamOutputModuleProperties(ves::open::xml::CommandPtr cmd);
    void SetParam(ves::open::xml::CommandPtr cmd);
    void SetLinkParam( ves::open::xml::CommandPtr cmd );
    */

    //Types::ArrayLong ids_;
    //CORBA::Long cur_id_;
    //std::string status_;
    //std::string data_;

    ///
    std::string UnitName_;

protected:

private:
    ///
    unsigned int m_returnState;

    ///
    CMainDlg* m_mainDialog;

    ///
    HWND m_powersimLog;

    ///
    CorbaUnitManager* m_corbaUnitManager;

    ///
    SIPParser* m_sipParser;

    ///
    std::string m_workingDir;

    ///
    std::string m_fileName;

    ///
    std::set< std::string > m_queryCommandNames;

};

#endif //POWERSIM_UNIT_I_H
