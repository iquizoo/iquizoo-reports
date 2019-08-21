SELECT DISTINCTROW
	interactivelog.userId AS user_id,
	interactivelog.excerciseId AS game_id,
	interactivelog.createTime AS game_time,
	excerciseownability.abId AS game_ability,
	interactivelog.standardScore AS game_score
FROM
	project
	INNER JOIN subproject ON subproject.projectId = project.id
	INNER JOIN interactivelog ON interactivelog.subId = subproject.id
	INNER JOIN excercise ON excercise.id = interactivelog.excerciseId
	INNER JOIN excerciseownability ON excerciseownability.excerciseId = excercise.id
WHERE
	project.keyword = "北京市脑计划";
