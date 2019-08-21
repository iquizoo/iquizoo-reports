SELECT DISTINCTROW
	subprojectownuser.userId AS user_id,
	userproperty.`key` AS prop_key,
	userownproperty.propertyValue AS prop_value
FROM
	project
	INNER JOIN subproject ON subproject.projectId = project.id
	INNER JOIN subprojectownuser ON subprojectownuser.sbId = subproject.id
	INNER JOIN userownproperty ON userownproperty.userId = subprojectownuser.userId
	INNER JOIN userproperty ON userproperty.id = userownproperty.propertyId
WHERE
	project.keyword = "北京市脑计划";
