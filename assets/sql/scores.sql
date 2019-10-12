SELECT
	user_profile.Id AS user_id,
	iquizoo_report_db.course_result_orginal.ContentId AS game_id,
	iquizoo_content_db.evaluation_games.`Name` AS game_name,
	iquizoo_report_db.course_result_orginal.CreateTime AS game_time,
	iquizoo_report_db.course_result_orginal.TimeConsuming AS game_duration,
	iquizoo_report_db.course_result_orginal.OrginalData AS game_data
FROM
	iquizoo_report_db.course_result_orginal
	INNER JOIN user_profile ON user_profile.Id = iquizoo_report_db.course_result_orginal.UserId
	INNER JOIN base_organization ON base_organization.Id = user_profile.OrgId
	INNER JOIN base_grade_class ON user_profile.ClassId = base_grade_class.Id
	INNER JOIN iquizoo_content_db.evaluation_games ON iquizoo_report_db.course_result_orginal.ContentId = iquizoo_content_db.evaluation_games.Id
WHERE
	base_organization.`Name` IN ( {organization_names} );
