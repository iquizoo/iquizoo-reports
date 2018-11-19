SELECT DISTINCTROW
	project.id AS project_id,
	project.`name` AS project_name,
	subproject.id AS subproject_id,
	subproject.`name` AS subproject_name,
	userownproperty.userId,
	userproperty.`key` AS prop_key,
	userownproperty.propertyValue AS prop_value
FROM
	project
	INNER JOIN subproject ON subproject.projectId = project.id
	INNER JOIN subprojectownuser ON subprojectownuser.sbId = subproject.id
	INNER JOIN userownproperty ON userownproperty.userId = subprojectownuser.userId
	INNER JOIN userproperty ON userproperty.id = userownproperty.propertyId
WHERE
	project.id IN ( {project_ids} );