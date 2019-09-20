-- !preview conn=iquizoo_db

SELECT
	user_profile.Id AS user_id,
	user_profile.RealName AS user_name,
	user_profile.Birthday AS dob,
	user_profile.Gender AS gender,
	base_organization.`Name` AS school,
	base_grade_class.GradeName AS grade,
	base_grade_class.ClassName AS class
FROM
	user_profile
	INNER JOIN base_organization ON user_profile.OrgId = base_organization.Id
	INNER JOIN base_grade_class ON user_profile.ClassId = base_grade_class.Id
WHERE
	base_organization.`Name` = "北京十一学校（龙樾实验中学）"
	AND base_grade_class.GradeName = "1年级";
