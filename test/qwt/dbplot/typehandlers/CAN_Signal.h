
#ifndef CAN_SIGNAL_H
#define CAN_SIGNAL_H

// --- Poco Includes --- //
#include <Poco/DynamicAny.h>

#include <Poco/Data/Common.h>

// --- STL Includes --- //
#include <string>


class CAN_Signal
{
public:
    ///Constructor
    CAN_Signal();

    ///Destructor
    ~CAN_Signal();

    ///
    std::string const& GetDefVal() const;

    ///
    Poco::DynamicAny const& GetMessageName() const;

    ///
    Poco::DynamicAny const& GetCanId() const;

    ///
    Poco::DynamicAny const& GetSignalName() const;

    ///
    Poco::DynamicAny const& GetStartBit() const;

    ///
    Poco::DynamicAny const& GetLength() const;

    ///
    Poco::DynamicAny const& GetFactor() const;

    ///
    Poco::DynamicAny const& GetUnit() const;

    ///We need this for set and multiset support
    bool operator < ( CAN_Signal const& cs ) const
    {
        return std::strtoul(
                   m_canId.convert< std::string >().c_str(), NULL, 16 ) <
               std::strtoul(
                   cs.m_canId.convert< std::string >().c_str(), NULL, 16 );
    }

    ///We need this operator to return the key for the map and multimap
    std::string operator () () const
    {
        return m_canId;
    }

protected:

private:
    ///
    friend class Poco::Data::TypeHandler< CAN_Signal >;

    ///
    void SetMessageName( std::string const& messageName );

    ///
    void SetCanId( std::string const& canId );

    ///
    void SetSignalName( std::string const& signalName );

    ///
    void SetStartBit( std::string const& startBit );

    ///
    void SetLength( std::string const& length );

    ///
    void SetFactor( std::string const& factor );

    ///
    void SetUnit( std::string const& unit );

    ///
    std::string m_defVal;

    ///
    Poco::DynamicAny m_messageName;

    ///
    Poco::DynamicAny m_canId;

    ///
    Poco::DynamicAny m_signalName;

    ///
    Poco::DynamicAny m_startBit;

    ///
    Poco::DynamicAny m_length;

    ///
    Poco::DynamicAny m_factor;

    ///
    Poco::DynamicAny m_unit;

};


namespace Poco
{
namespace Data
{

template<>
class TypeHandler< CAN_Signal >
{
public:
    ///
    static std::size_t size()
    {
        //We handle seven columns of the Tables
        return 7;
    }

    ///
   static void bind(
       std::size_t pos,
       CAN_Signal const& obj,
       AbstractBinder* pBinder )
    {
        poco_assert_dbg( pBinder != 0 );

        //Note that we advance pos by the number of columns the datatype uses!
        //For string/int this is one
        TypeHandler< std::string >::bind(
            pos++, obj.GetMessageName().convert< std::string >(), pBinder );
        TypeHandler< std::string >::bind(
            pos++, obj.GetCanId().convert< std::string >(), pBinder );
        TypeHandler< std::string >::bind(
            pos++, obj.GetSignalName().convert< std::string >(), pBinder );
        TypeHandler< std::string >::bind(
            pos++, obj.GetStartBit().convert< std::string >(), pBinder );
        TypeHandler< std::string >::bind(
            pos++, obj.GetLength().convert< std::string >(), pBinder );
        TypeHandler< std::string >::bind(
            pos++, obj.GetFactor().convert< std::string >(), pBinder );
        TypeHandler< std::string >::bind(
            pos++, obj.GetUnit().convert< std::string >(), pBinder );
    }

    ///
    static void prepare(
        std::size_t pos,
        CAN_Signal const& obj,
        AbstractPreparation* pPrepare )
    {
        poco_assert_dbg( pPrepare != 0 );

        //Note that we advance pos by the number of columns the datatype uses!
        //For string/int this is one
        TypeHandler< std::string >::prepare(
            pos++, obj.GetMessageName().convert< std::string >(), pPrepare );
        TypeHandler< std::string >::prepare(
            pos++, obj.GetCanId().convert< std::string >(), pPrepare );
        TypeHandler< std::string >::prepare(
            pos++, obj.GetSignalName().convert< std::string >(), pPrepare );
        TypeHandler< std::string >::prepare(
            pos++, obj.GetStartBit().convert< std::string >(), pPrepare );
        TypeHandler< std::string >::prepare(
            pos++, obj.GetLength().convert< std::string >(), pPrepare );
        TypeHandler< std::string >::prepare(
            pos++, obj.GetFactor().convert< std::string >(), pPrepare );
        TypeHandler< std::string >::prepare(
            pos++, obj.GetUnit().convert< std::string >(), pPrepare );
    }

    ///obj will contain the result, defVal contains values we should use when one column is NULL
    static void extract(
        std::size_t pos,
        CAN_Signal& obj,
        CAN_Signal const& defVal,
        AbstractExtractor* pExt )
    {
        poco_assert_dbg( pExt != 0 );

        std::string const& dVal = defVal.GetDefVal();

        std::string messageName;
        TypeHandler< std::string >::extract(
            pos++, messageName, dVal, pExt );
        obj.SetMessageName( messageName );

        std::string canId;
        TypeHandler< std::string >::extract(
            pos++, canId, dVal, pExt );
        obj.SetCanId( canId );

        std::string signalName;
        TypeHandler< std::string >::extract(
            pos++, signalName, dVal, pExt );
        obj.SetSignalName( signalName );

        std::string startBit;
        TypeHandler< std::string >::extract(
            pos++, startBit, dVal, pExt );
        obj.SetStartBit( startBit );

        std::string length;
        TypeHandler< std::string >::extract(
            pos++, length, dVal, pExt );
        obj.SetLength( length );

        std::string factor;
        TypeHandler< std::string >::extract(
            pos++, factor, dVal, pExt );
        obj.SetFactor( factor );

        std::string unit;
        TypeHandler< std::string >::extract(
            pos++, unit, dVal, pExt );
        obj.SetUnit( unit );
    }

};

} //end Data
} //end Poco

#endif //CAN_SIGNAL_H
