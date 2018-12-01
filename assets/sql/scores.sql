SELECT DISTINCTROW
	project.id AS project_id,
	project.`name` AS project_name,
	subproject.id AS subproject_id,
	subproject.`name` AS subproject_name,
	examresult.examId,
	examresult.userId,
	examresult.createDate,
	examresult.bci_score AS bci,
	examresultdetail.abId,
	examresultdetail.score
FROM
	project
	INNER JOIN subproject ON subproject.projectId = project.id
	INNER JOIN examresult ON examresult.subProjectId = subproject.id
	INNER JOIN examresultdetail ON examresultdetail.resultId = examresult.id
WHERE
	project.id IN ( {customer_projectids} );
