-- !preview conn=iquizoo_user_db

SELECT
  user_profile.Id AS user_id,
  user_profile.RealName AS user_name,
  user_profile.Gender AS gender,
  user_profile.Birthday AS dob,
  base_organization.Name AS school,
  base_grade_class.GradeName AS grade,
  base_grade_class.ClassName AS class
FROM
  user_profile
  INNER JOIN base_organization ON base_organization.Id = user_profile.OrgId
  INNER JOIN base_grade_class ON base_grade_class.Id = user_profile.ClassId
WHERE
  base_organization.Id IN ( {customer_projectids} );
