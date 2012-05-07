#pragma once

#include <string>
#include <boost/any.hpp>

namespace Persistence
{

class SearchCriterion
{
public:

    enum OPERATOR { AND, OR, LIKE, BETWEEN };

    SearchCriterion(  ):
        m_isOperatorCriterion( false )
    {

    }

    SearchCriterion( OPERATOR op  ):
        m_isOperatorCriterion( true )
    {

    }

    SearchCriterion( const std::string& key,
                     const std::string& comparison,
                     boost::any value ):
        m_isOperatorCriterion( false ),
        m_key( key ),
        m_comparison( comparison ),
        m_value( value )
    {

    }

    bool m_isOperatorCriterion;
    std::string m_key;
    std::string m_comparison;
    boost::any m_value;
};

} // Persistence
