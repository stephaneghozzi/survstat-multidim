library(shiny)
library(RCurl)
library(XML)
library(stringi)
library(data.table)  
library(ISOweek)
library(dplyr)
library(tidyr)

##### functions
# method to recieve the xml from the WebService
# parameter: body_ : the xml request,  service_ : the WebService method
getXMLFromWebService <- function(body_,service_){
  
  # URL to the WebService
  webServiceUrl <- "https://tools.rki.de/SurvStat/SurvStatWebService.svc"
  
  bodyLength <- nchar( stri_enc_toutf8(body_, is_unknown_8bit = FALSE, validate = FALSE))
  
  # Header Fields for the post  
  headerFields =
    c(Accept = "text/xml",
      Accept = "multipart/*",
      'Content-Length' = bodyLength,
      'Content-Type' = "application/soap+xml; charset=utf-8", # Extern tools.rki.de
      SOAPaction = paste0("http://tools.rki.de/SurvStat/SurvStatWebService/", service_))
  
  #Gatherer Object to recieve response xml
  reader = basicTextGatherer()
  
  # performe the request
  result <- curlPerform(url = webServiceUrl,
                        httpheader = headerFields,
                        postfields = body_,
                        writefunction = reader$update,
                        verbose = FALSE
  )
  
  # parse the response xml 
  doc_x <- xmlParse(reader$value(), encoding="UTF-8")
  
  #return xml 
  return(doc_x)
}

getHierarchyMembers <- function(cube, language, filter){
  # Select HierarchyId from the first row
  hId <- getHierarchies(cube, language) %>% filter(HierarchyCaption==filter) %>% dplyr::select(HierarchyId) %>% unlist()
  
  # XML Request Body 
  body = paste0('<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:sur="http://tools.rki.de/SurvStat/" xmlns:rki="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx">
                  <soap:Header xmlns:wsa="http://www.w3.org/2005/08/addressing"> 
                  <wsa:Action>http://tools.rki.de/SurvStat/SurvStatWebService/GetAllHierarchyMembers</wsa:Action>
                  <wsa:To>https://tools.rki.de/SurvStat/SurvStatWebService.svc</wsa:To>
                  </soap:Header>
                  <soap:Body>
                  <sur:GetAllHierarchyMembers>
                  <sur:request>
                  <rki:Cube>', cube,'</rki:Cube>
                  <rki:HierarchyId>',hId,'</rki:HierarchyId>
                  <rki:Language>', language,'</rki:Language>
                  </sur:request>
                  </sur:GetAllHierarchyMembers>
                  </soap:Body>
                  </soap:Envelope>')
  
  # XML Request Body 
  service_ <- 'GetAllHierarchyMembers'
  
  # WebService method
  AllHierarchyMembers <- getXMLFromWebService(body,service_)
  
  # Get NodeSet of all Hierarchy Members Nodes in the response xml
  hierarchyMembers <- getNodeSet(AllHierarchyMembers, "//a:HierarchyMember", namespaces  =  c("a" = "http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx"))
  
  # Parse the XML Result to DataFrame
  HierarchyMemberDataFrame <-xmlToDataFrame(hierarchyMembers)
  
  return(HierarchyMemberDataFrame)
}

getHierarchies <- function(cube, language){
  #language = 'German' #'German'/'English' (Case Sensitive!) 
  #cube = 'SurvStat' #'SurvStat' (Case Sensitive!)
  # XML Request Body -------------EXTERN
  body =paste0('<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:sur="http://tools.rki.de/SurvStat/" xmlns:rki="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx">
                 <soap:Header xmlns:wsa="http://www.w3.org/2005/08/addressing">
                 <wsa:Action>http://tools.rki.de/SurvStat/SurvStatWebService/GetAllDimensions</wsa:Action>
                 <wsa:To>https://tools.rki.de/SurvStat/SurvStatWebService.svc</wsa:To>
                 </soap:Header>
                 <soap:Body>
                 <sur:GetAllDimensions>      
                 <sur:request>           
                 <rki:Cube>', cube,'</rki:Cube>          
                 <rki:Language>', language,'</rki:Language>
                 </sur:request>
                 </sur:GetAllDimensions> 
                 </soap:Body>
                 </soap:Envelope>')
  
  # WebService method
  service_ <- 'GetAllDimensions'
  
  # Call getXMLFromWebService to get response xml
  AllDimensions <- getXMLFromWebService(body,service_)
  
  # Get NodeSet of all Hierarchy Nodes in the response xml
  idNodes <- getNodeSet(AllDimensions, "//b:Hierarchy", namespaces  =  c("b" = "http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx"))
  
  # Build Hierarchy DataFrame
  HierarchyDataFrame <- do.call(rbind.data.frame, lapply(idNodes, function(x) {
    # Get the Children Nodes of the Hierarchy Node (These are its properties)
    children <- xmlChildren(x)
    
    # Get the parent of the parent Node (ParentÂ² because every Hierarchy is in in Hierachies Node)
    parent <- xmlParent(xmlParent(x))
    
    # Initialize the result List
    result=list() 
    
    # Fill the ResultList with the Hierarchy Child "Properties"
    result$HierarchyId <-xmlValue(children$Id)
    result$HierarchyCaption <- xmlValue(children$Caption)
    result$HierarchyDescription <-xmlValue(children$Description)
    result$HierarchySelectMax <-xmlValue(children$SelectMax)
    result$HierarchySelectMin <-xmlValue(children$SelectMin)
    result$HierarchySort <-xmlValue(children$Sort)
    #If Parent is a Hierarchy --> Set den Parent Hierarchy Id
    result$HierarchyParentId = ifelse(xmlName(parent)=="Hierarchy",xmlValue(xmlChildren(parent)$Id),-1)
    
    #Loop until the Parent is the Dimension and set it as Dimension Parent
    while(xmlName(parent) != "Dimension"){
      parent  <- xmlParent(parent)
    }
    
    #Get the Children Nodes (Its Properties) of the Dimenionnode
    dimensionChilds<- xmlChildren(parent)
    
    # Fill the ResultList with the Dimension Child "Properties"
    result$DimensionId <- xmlValue(dimensionChilds$Id)
    result$DimensionCaption <- xmlValue(dimensionChilds$Caption)
    result$DimensionDescription <- xmlValue(dimensionChilds$Description)
    result$DimensionGroup <- xmlValue(dimensionChilds$Group)
    result$DimensionSort <- xmlValue(dimensionChilds$Sort)
    
    # Return the result List
    
    return(result)
  }))
  
  return(HierarchyDataFrame)
  
}


getOlapData_MD_Filter <- function(cube, language, hierarchy, facet, filters, filterValues){
  
  column <- getHierarchies(cube, language) %>% filter(HierarchyCaption==hierarchy) %>% select(HierarchyId) %>% unlist() %>% as.character()
  row <- getHierarchies(cube, language) %>% filter(HierarchyCaption==facet) %>% select(HierarchyId) %>% unlist() %>% as.character()

  filter_collection = list()
  for(i in (1:length(filters))){
    filter = filters[i]
    filterValue = filterValues[i]
    
    keyDim <- getHierarchies(cube, language) %>% filter(HierarchyCaption==filter) %>% select(DimensionId) %>% unlist() %>% as.character()
    keyHier <- getHierarchies(cube, language) %>% filter(HierarchyCaption==filter) %>% select(HierarchyId) %>% unlist() %>% as.character()
    value <- getHierarchyMembers(cube, language, filter) %>% filter(Caption==filterValue) %>% select(Id) %>% unlist() %>% as.character() %>% gsub('&', '&amp;', .)
  
    filter_collection[[i]] = list("keyDim"=keyDim,"keyHier"=keyHier,"value"=value)
  }
  
  filter_collection_string = paste(lapply(filter_collection, function(x) {
    paste0('<q13:KeyValueOfFilterCollectionKeyFilterMemberCollectionb2rWaiIW>
          <q13:Key>
          <q13:DimensionId>', x["keyDim"] ,'</q13:DimensionId>
          <q13:HierarchyId>', x["keyHier"] ,'</q13:HierarchyId>
          </q13:Key>
          <q13:Value>
          <q13:string>', x["value"] ,'</q13:string>
          </q13:Value>
          </q13:KeyValueOfFilterCollectionKeyFilterMemberCollectionb2rWaiIW>\n')}),collapse="")
  
  body = paste0('<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"  xmlns:tns="http://tools.rki.de/SurvStat/" xmlns:msc="http://schemas.microsoft.com/ws/2005/12/wsdl/contract" xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd" xmlns:q1="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q2="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q3="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q4="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q5="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q6="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q7="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q8="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q9="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q10="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q11="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q12="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q13="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q14="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q15="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:q16="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx" xmlns:ser="http://schemas.microsoft.com/2003/10/Serialization/">
<soap:Header>
                  <wsa:To xmlns:wsa="http://www.w3.org/2005/08/addressing" xmlns="http://www.w3.org/2005/08/addressing">https://tools.rki.de/SurvStat/SurvStatWebService.svc</wsa:To>
                  <wsa:Action xmlns:wsa="http://www.w3.org/2005/08/addressing" xmlns="http://www.w3.org/2005/08/addressing">http://tools.rki.de/SurvStat/SurvStatWebService/GetOlapResultData</wsa:Action>
                  </soap:Header>
                  <soap:Body>
                  <GetOlapResultData xmlns="http://tools.rki.de/SurvStat/">
                  <request>
                  <q13:ColumnHierarchy xmlns:q13="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx">',column,'</q13:ColumnHierarchy>
                  <q13:Cube xmlns:q13="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx">SurvStat</q13:Cube>
                  <q13:HierarchyFilters xmlns:q13="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx">\n',
                  filter_collection_string
                  ,'</q13:HierarchyFilters>
                  <q13:IncludeNullColumns xmlns:q13="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx">false</q13:IncludeNullColumns>
                  <q13:IncludeNullRows xmlns:q13="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx">false</q13:IncludeNullRows>
                  <q13:IncludeTotalColumn xmlns:q13="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx">false</q13:IncludeTotalColumn>
                  <q13:IncludeTotalRow xmlns:q13="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx">false</q13:IncludeTotalRow><q13:Language xmlns:q13="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx">German</q13:Language>
                  <q13:Measure xmlns:q13="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx">[Measures].[FallCount_71]</q13:Measure>
                  <q13:RowHierarchy xmlns:q13="http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx">',row,'</q13:RowHierarchy>
                  </request>
                  </GetOlapResultData>
                  </soap:Body>
                  </soap:Envelope>')
  
  # WebService method
  service_ <- 'GetOlapResultData'
  
  AllData <- getXMLFromWebService(body,service_)
  
  dataNodeColumnSet <- getNodeSet(AllData, "//b:Columns/b:QueryResultColumn", namespaces  =  c("b" = "http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx"))
  columnCaptions <- lapply(dataNodeColumnSet, function(x) {
    columnCaption <- xmlValue(getNodeSet(x, "./b:Caption", namespaces  =  c("b" = "http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx"))[[1]])
    
  })
  
  dataNodeRowSet <- getNodeSet(AllData, "//b:QueryResultRow", namespaces  =  c("b" = "http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx"))
  rowDataSet <- do.call(rbind.data.frame, lapply(dataNodeRowSet, function(x) {
    vals <- getNodeSet(x, "./b:Values/b:string", namespaces  =  c("b" = "http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx"))
    lapply(vals, function(x) { as.numeric(gsub("\\.", "", xmlValue(x)))})
  }))
  rowCaptions <- getNodeSet(AllData, "//b:QueryResults/b:QueryResultRow/b:Caption", namespaces  =  c("b" = "http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx"))
  rowCaptions <-lapply(rowCaptions, function(x) {xmlValue(x)})
  
  if(nrow(rowDataSet)==0){return(data.frame())}
  
  row.names(rowDataSet) <-rowCaptions
  trowSet <- as.data.frame(t(rowDataSet))
  rownames(trowSet) <- 1:nrow(trowSet)
  
  columnCaptionsT <- data.frame(t(data.frame(columnCaptions)))
  names(columnCaptionsT) <- "Categories"
  finalFrame <- data.frame(cbind(columnCaptionsT,trowSet))
  rownames(finalFrame) <- 1:nrow(finalFrame)
  
  data.m = pivot_longer(finalFrame, cols = -one_of(c("Categories")))
  #data.m <- data.table::melt(finalFrame, id.vars='Categories') 
  
  data.m
  
}

get_diseases <- function(){
  cube <- "SurvStat"
  language <- "English"
  filter <- "Disease"
  getHierarchyMembers(cube, language, filter) %>% dplyr::select(Caption)
}

get_strata <- function(cube, language, facets){
  
  if(length(facets)==1){
    res = as.data.frame(getHierarchyMembers("SurvStat", "English", facets[[1]])[,"Caption"])
    colnames(res) = c(facets[[1]])
    return(res)
  }
  
  one_w_many <- function(one, many) lapply(many, function(x) c(one,x))
  
  # infix operator for cartesian product:
  "%p%" <- function( a, b ) {
    p = c( sapply(a, function (x) one_w_many(x, b) ) )
    if (is.null(unlist(p))) list() else p}
  
  
  go <- function (...) {
    Reduce( '%p%', ... )
  }
  
  vectorlist = go(lapply(facets, function(f) as.character(getHierarchyMembers("SurvStat", "English", f)$Caption)))
  df = as.data.frame(do.call(rbind, vectorlist))
  colnames(df) = facets
  df
}


get_weekly_timeseries <- function(disease = "Noroviral gastroenteritis", facet = "County", filters, filterValues){
  cube <- "SurvStat"
  language <- "English"
  hierarchy <- "Year and week of notification"
  
  filters = c("Disease",filters)
  filterValues = c(disease,filterValues)
  
  data <- getOlapData_MD_Filter(cube, language, hierarchy, facet, filters, filterValues)
  if(nrow(data)==0){
    return(data)}
  
  data$Year <- substr(data$Categories,1,4)
  data$Week <- substr(data$Categories,8,9)
  data$date <- ISOweek::ISOweek2date(paste0(data$Year,"-W",data$Week,"-1"))
  
  current_colnames = colnames(data)
  current_colnames[2] = facet
  current_colnames[3] = "Count"
  colnames(data) = current_colnames
  
  for(i in (3:length(filters))){
    data[filters[i]] = filterValues[i]
  }
  
  data
}

get_weekly_timeseries_stratified <- function(disease = "Noroviral gastroenteritis", facet = "County", strata_frame, save_path=NULL, save_every=50){
  print(colnames(strata_frame))
  df_list = list()
  counter = 1
  
  for(i in 1:nrow(strata_frame)){
     x = strata_frame[i,]
     df = get_weekly_timeseries(disease=disease,
       facet=facet,
       filters = colnames(strata_frame),
       filterValues = as.character(unname(unlist(x))))
     
     print(paste0(round(100*i/nrow(strata_frame),2),"%: ",nrow(df)," rows found for ",paste(as.character(unname(unlist(x))),collapse=", ")))
     
     if(nrow(df)>0){
       df_list[[counter]]=df
     }
     
     if(i%%save_every==0 & !is.null(save_path)){
       all_df_current = do.call(rbind,df_list)
       saveRDS(all_df_current,save_path)
       print(paste("Saved",nrow(all_df_current),"rows in temporary file."))
       df_list = list()
       counter=1
       df_list[[counter]]=all_df_current
     }
     
     counter = counter + 1
  }
  #if(any(sapply(df_list, nrow) == 0)){
  #no_rows <- which(sapply(df_list, nrow) == 0)
  #df_list = df_list[-no_rows]
  #}
  
  all_df_current = do.call(rbind,df_list)
  saveRDS(all_df_current,save_path)
  print(paste("Saved",nrow(all_df_current),"rows in temporary file."))
  return(all_df_current)
}

get_County_State_mapping = function(){
  res = get_weekly_timeseries_stratified(strata_frame=strat)
  county_state = res %>% select(name,State) %>% distinct()
  return(county_state)
}