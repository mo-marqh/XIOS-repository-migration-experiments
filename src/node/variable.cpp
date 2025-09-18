#include "variable.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "object_factory.hpp"
#include "xios_spl.hpp"
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include <boost_extract.hpp>

namespace xios {

   /// ////////////////////// Définitions ////////////////////// ///

   CVariable::CVariable(CContext* context)
      : CObjectTemplate<CVariable>(context)
      , CVariableAttributes()
      , content()
   { /* Ne rien faire de plus */ }

   CVariable::CVariable(CContext* context, const StdString & id)
      : CObjectTemplate<CVariable>(context, id)
      , CVariableAttributes()
      , content()
   { /* Ne rien faire de plus */ }

   CVariable::~CVariable(void)
   { /* Ne rien faire de plus */ }

   StdString CVariable::GetName(void)   { return (StdString("variable")); }
   StdString CVariable::GetDefName(void){ return (CVariable::GetName()); }
   ENodeType CVariable::GetType(void)   { return (eVariable); }

   void CVariable::parse(xml::CXMLNode & node)
   {
      SuperClass::parse(node);
      StdString id = (this->hasId()) ? this->getId() : StdString("undefined");
      if (context_->getId()=="xios") checkInDictionary(id);
      if (!node.getContent(this->content))
      {
        xml::THashAttributes attributes = node.getAttributes();
        StdString variableName = attributes["name"];

        node.goToParentElement();
        StdString parentName = node.getElementName();
        attributes = node.getAttributes();
        error << "The variable id = " << id << " and name = " << variableName << " does not have any content. Please define it!" << std::endl
              << "This variable is inside another element whose attributes are :" << std::endl;

        for (xml::THashAttributes::iterator it = attributes.begin(); it != attributes.end(); ++it)
        {
           error << it->first << "=\"" << it->second.c_str() << "\" ";
        }
        error << std::endl; 

         ERROR("CVariable::parse(xml::CXMLNode & node)",
               << "[ variable id = " << id
               << " ] variable is not defined !. It does not have any content. See error log for more details.");
      }
      content = xios_trim_copy(content) ;
   }

   void CVariable::checkInDictionary(StdString id)
   {
     // $ grep -rw getin ../src/|grep -v template|grep \"|awk -F \" '{ print $2}'|sort|uniq
     set<StdString> dictionary_ = { 
                                    "buffer_size_factor",
                                    "call_oasis_enddef",
                                    "check_event_sync",
                                    "checksum_recv_fields",
                                    "checksum_send_fields",
                                    "clients_code_id",
                                    "info_level",
                                    "log_memory",
                                    "log_type",
                                    "max_buffer_size",
                                    "memory_report",
                                    "memtrack_blocks",
                                    "memtrack_size",
                                    "min_buffer_size",
                                    "number_pools_server2",
                                    "number_prefetched_requests",
                                    "optimal_buffer_size",
                                    "print_file",
                                    "pure_one_sided",
                                    "ratio_server2",
                                    "recv_field_timeout",
                                    "server2_dist_file_memory",
                                    "server2_dist_file_memory_ratio",
                                    "server_puplish_timeout",
                                    "system_stack",
                                    "transport_protocol",
                                    "using_oasis",
                                    "using_server",
                                    "using_server2",
                                    "xios_stack",
                                    "services_use_window_manager",
                                    "max_attached_mem_window"                        
     };
     if (dictionary_.find(id)==dictionary_.end())
     {
       ERROR("CVariable::checkInDictionary(StdString id)",
              << "[ variable id = " << id
              << " ] variable does not exist ! Check the syntax of your context id 'xios' in iodef.xml.");
     }
   }

   const StdString& CVariable::getVariableOutputName(void) const
   {
     return name.isEmpty() ? getId() : name;
   }

   const StdString & CVariable::getContent (void) const
   {
      return (this->content);
   }

   void CVariable::setContent(const StdString& contentStr)
   {
     this->content = contentStr;
   }

   StdString CVariable::toString(void) const
   {
      StdOStringStream oss;

      oss << "<" << CVariable::GetName() << " ";
      if (this->hasId())
         oss << " id=\"" << this->getId() << "\" ";
      oss << SuperClassAttribute::toString() << ">" << std::endl
          << this->content /*<< std::endl*/;
      oss << "</" << CVariable::GetName() << " >";
      return (oss.str());
   }

   /*
   *\brief Sending value of a variable with its id from client to server
   *
   */

   void CVariable::sendValue(CContextClient* client)
   {
     CEventClient event(this->getType(),EVENT_ID_VARIABLE_VALUE) ;
     if (client->isServerLeader())
     {
       CMessage msg ;
       msg<<this->getId() ;
       msg<<content ;
       const std::list<int>& ranks = client->getRanksServerLeader();
       for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
         event.push(*itRank,1,msg);
       client->sendEvent(event) ;
     }
     else client->sendEvent(event) ;
   }

   /*
   *\brief Receive value of a variable with its id from client to server
   *
   */
   void CVariable::recvValue(CContext* context, CEventServer& event)
   {
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      get(context, id)->recvValue(*buffer);
   }

   /*
   *\brief Receive value of a variable with its id from client to server
   *
   */
   void CVariable::recvValue(CBufferIn& buffer)
   {
      string str ;
      buffer>>str;
      setContent(str);
   }

   bool CVariable::dispatchEvent(CContext* context, CEventServer& event)
   {
    if (SuperClass::dispatchEvent(context, event)) return true ;
    else
    {
      switch(event.type)
      {
        case EVENT_ID_VARIABLE_VALUE :
          recvValue(context, event) ;
          return true ;
          break ;

        default :
          ERROR("bool CVariable::dispatchEvent(CEventServer& event)",<<"Unknown Event") ;
          return false ;
      }
    }
   }

/*
   void CVariable::toBinary(StdOStream & os) const
   {
     const StdString & content = this->content;
     const StdSize size        =  content.size();
     SuperClass::toBinary(os);

     os.write (reinterpret_cast<const char*>(&size), sizeof(StdSize));
     os.write (content.data(), size * sizeof(char));
   }

   void CVariable::fromBinary(StdIStream & is)
   {
      SuperClass::fromBinary(is);
      StdSize size  = 0;
      is.read (reinterpret_cast<char*>(&size), sizeof(StdSize));
      this->content.assign(size, ' ');
      is.read (const_cast<char *>(this->content.data()), size * sizeof(char));
   }
*/
   void CVariableGroup::parse(xml::CXMLNode & node, bool withAttr)
   {
      CVariableGroup* group_ptr = (this->hasId())
         ? CVariableGroup::get(context_, this->getId()) : CVariableGroup::get(context_, this);

      StdString content;
      if (this->getId().compare(CVariableGroup::GetDefName()) != 0 && node.getContent(content))
      {
        StdSize beginid = 0, endid = 0, begindata = 0, enddata = 0;
        StdString subdata, subid;

        while ((beginid = content.find_first_not_of ( " \r\n\t;", enddata)) != StdString::npos)
        {
           endid   = content.find_first_of ( " \r\n\t=", beginid );
           subid   = content.substr ( beginid, endid-beginid);
           subid   = xios_to_lower_copy(xios_trim_copy(subid)) ;

           begindata = content.find_first_of ( "=", endid ) + 1;
           enddata   = content.find_first_of ( ";", begindata );
           subdata   = content.substr ( begindata, enddata-begindata);
           subdata   = xios_trim_copy(subdata) ;
           group_ptr->createChild(subid)->content = subdata ;
        }
      }
      else
      {
         SuperClass::parse(node, withAttr);
      }
      //SuperClass::parse(node, withAttr);

   }

} // namespace xios
